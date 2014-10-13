{-
   Copyright (c) Microsoft Corporation
   All rights reserved.

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
-}
{-# OPTIONS_GHC -Wall #-}
module TcComp (tyCheckTopComp, tyCheckTopDecls, envOfDecls) where

import Control.Applicative
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust)
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJ
import qualified Data.List as L
import qualified Data.Set as S

import AstComp
import AstExpr
import CtComp (ctComp) -- Used only at very top-level (single call)
import Outputable
import PpComp ()
import PpExpr ()
import TcErrors
import TcExpr
import TcMonad
import TcUnify

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | Free variables
--
-- NOTE: We ignore annotations on bound occurrences of computation variables
tyCheckCFree :: GName (Maybe (GCTy SrcTy)) -> TcM (GName CTy, CTy)
tyCheckCFree nm = do
  cty <- lookupCEnv (name nm) (nameLoc nm)
  return (nm{nameTyp = cty}, cty)

-- | Bound variables
--
-- NOTE: We do not support type inference for computation variables, so we
-- must be passed the type of the variable. The annotation on the variable is
-- ignored.
--
-- TODO: If we do support CTy annotations on variables we should unify the
-- annotation (if one exists) with the type that is passed in.
tyCheckCBound :: GName (Maybe (GCTy SrcTy)) -> CTy -> TcM (GName CTy)
tyCheckCBound nm cty = return nm{nameTyp = cty}

{-------------------------------------------------------------------------------
  Computations
-------------------------------------------------------------------------------}

-- | Computations
--
-- TODO: We currently ignore any type annotations (in Emit, Emits, Return, etc.)
-- since we don't give the user any syntax for providing these. It wouldn't be
-- difficult to support them (call unifyAnn) but it's probably unnecessary.
tyCheckComp :: SrcComp -> TcM (Comp, CTy)
tyCheckComp c
  = do { let cloc = compLoc c
       ; pushErrCtx (CompErrCtx c) $
         case unComp c of
           Var x ->
             do { (x', cty) <- tyCheckCFree x
                ; return (cVar cloc () x', cty)
                }

           BindMany c1 [] ->
             tyCheckComp c1
           BindMany c1 ((x,c2):rest) ->
             do { (c1', cty1) <- tyCheckComp c1

                ; checkWith (compLoc c1) (hasDoneTyBase cty1) $
                    expectedButFound "computer" "transformer" c1

                ; (x', xty) <- tyCheckBound x
                ; unify cloc xty (fromJust $ doneTyOfCTyBase cty1)

                ; (c', cty0) <- extendEnv [x'] $
                     tyCheckComp (MkComp (mkBindMany c2 rest) cloc ())

                ; unify cloc ( inTyOfCTyBase cty1) ( inTyOfCTyBase cty0)
                ; unify cloc (yldTyOfCTyBase cty1) (yldTyOfCTyBase cty0)

                ; return (MkComp (mkBind c1' (x',c')) cloc (), cty0)
                }

           Seq c1 c2 ->
             do { nm <- genSym "_x"
                ; tyCheckComp $
                    MkComp (mkBind c1 (toName nm Nothing Nothing, c2)) cloc ()
                }

           Par parInfo c1 c2 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    CTBase (TTrans a b)  ->
                        do { (c2', cty2) <- tyCheckComp c2
                           ; case cty2 of
                               CTBase (TTrans b' t) ->
                                 do { unify cloc b b'
                                    ; let cTy = CTBase (TTrans a t)
                                    ; return (cPar cloc () parInfo c1' c2', cTy)
                                    }
                               CTBase (TComp u b' t) ->
                                 do { unify cloc b b'
                                    ; let cTy = CTBase (TComp u a t)
                                    ; return (cPar cloc () parInfo c1' c2', cTy)
                                    }
                               CTArrow _ _
                                 -> raiseErrNoVarCtx cloc (nonFullAppErr c2)
                           }
                    CTBase (TComp v a b) ->
                        do { (c2', cty2) <- tyCheckComp c2
                           ; case cty2 of
                               CTBase (TTrans b' t) ->
                                 do { unify cloc b b'
                                    ; let cTy = CTBase (TComp v a t)
                                    ; return (cPar cloc () parInfo c1' c2', cTy)
                                    }
                               CTBase (TComp _ _ _) ->
                                 do { cty1' <- zonkCTy cty1
                                    ; cty2' <- zonkCTy cty2
                                    ; raiseErrNoVarCtx cloc $
                                      vcat [ text "Computer-Computer (>>>) composition"
                                           , text "Left computer:"
                                           , nest 2 (ppr c1')
                                           , text "Type:" <+> ppr cty1'
                                           , text "Right computer:"
                                           , nest 2 (ppr c2')
                                           , text "Type:" <+> ppr cty2'
                                           ]
                                    }
                                 -- NB: To enable computer-computer
                                 -- composition uncomment the following:
                                 -- do { unify cloc u v
                                 --    ; unify cloc b b'
                                 --    ; let cTy = CTBase (TComp u a t)
                                 --    ; return $ cPar cloc cTy parInfo c1' c2'
                                 --    }
                               CTArrow _ _
                                 -> raiseErrNoVarCtx cloc (nonFullAppErr c2)
                           }
                    CTArrow {} -> raiseErrNoVarCtx cloc (nonFullAppErr c1)
                }


           Let x c1 c2 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; x' <- tyCheckCBound x cty1
                ; (c2', cty2) <- extendCEnv [x'] $ tyCheckComp c2
                ; return (cLet cloc () x' c1' c2', cty2)
                }

           LetStruct sdef c2 ->
             do { sdef' <- trStructDef cloc sdef
                ; (c2', cty2) <- extendTDefEnv [(struct_name sdef,sdef')] $
                                   tyCheckComp c2
                ; return (cLetStruct cloc () sdef' c2', cty2)
                }

           LetE x fi e c1 ->
             do { (x', xty) <- tyCheckBound x
                ; (e', ety) <- tyCheckExpr e
                ; unify cloc xty ety
                ; (c1', cty1) <- extendEnv [x'] $ tyCheckComp c1
                ; return (cLetE cloc () x' fi e' c1', cty1)
                }

           LetERef x (Just e) c1 ->
             do { (x', xty) <- tyCheckBound x
                ; (e', ety) <- tyCheckExpr e
                ; unify cloc xty ety
                ; (c1', cty1) <- extendEnv [x'] $ tyCheckComp c1
                ; return (cLetERef cloc () x' (Just e') c1', cty1)
                }

           LetERef x Nothing c1 ->
             do { (x',  _)    <- tyCheckBound x
                ; (c1', cty1) <- extendEnv [x'] $ tyCheckComp c1
                ; return (cLetERef cloc () x' Nothing c1', cty1)
                }

           LetHeader fn c1 ->
             do { fn' <- tyCheckFun fn
                ; (c1', cty1) <- extendEnv [funName fn'] $ tyCheckComp c1
                ; return (cLetHeader cloc () fn' c1', cty1)
                }

           LetFunC f params locls c1 c2 ->
             do { params' <- mapM tyCheckParam params
                ; let (eparams, cparams) = partitionEithers $ map classify params'
                ; locls' <- extendEnv eparams $ tyCheckDecls locls
                -- NB: order in which Env is extended is important here,
                -- for shadowing
                ; let env = eparams ++ map fst locls'
                ; (c1', tyc1) <- extendEnv env $ extendCEnv cparams $ tyCheckComp c1
                ; case tyc1 of
                    CTBase cty0 ->
                      do { fty <- mkFunTy params' cty0
                         ; f' <- tyCheckCBound f fty
                         ; (c2', tyc2) <- extendCEnv [f'] $ tyCheckComp c2
                         ; return (cLetFunC cloc () f' params' locls' c1' c2', tyc2)
                         }
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }
              where
                classify :: GName (CallArg Ty CTy) -> Either (GName Ty) (GName CTy)
                classify nm = case nameTyp nm of
                                 CAExp  t -> Left  nm{nameTyp = t}
                                 CAComp t -> Right nm{nameTyp = t}

                mkFunTy :: [GName (CallArg Ty CTy)] -> CTy0 -> TcM CTy
                mkFunTy args res = do
                  args' <- forM args $ \nm -> case nameTyp nm of
                    CAExp t ->
                      return $ CAExp t
                    CAComp (CTBase t) ->
                      return $ CAComp t
                    CAComp (CTArrow _ _) ->
                      raiseErrNoVarCtx cloc $
                        text "Higher order computation arguments not supported"
                  return $ CTArrow args' res

           Call f actual ->
             do { (f', fun_ty) <- tyCheckCFree f
                ; checkWith cloc (not (isCTyBase fun_ty)) $
                  vcat [ text "Computation " <+> ppr f
                       , text "applied, but has non-arrow type:" <+>
                         ppr fun_ty
                       ]
                ; let CTArrow formal res = fun_ty

                ; checkWith cloc (length formal == length actual) $
                  vcat [ text "Computation function" <+> ppr f
                       , text "Expecting" <+> int (length formal) <+> text "arguments"
                       , text "but was given" <+> int (length actual)
                       ]

                ; (actualEs', actualTys') <- unzip <$> mapM tyCheckCallArg actual

                ; let check_call_arg :: ( CallArg Ty CTy0
                                        , CallArg SrcExp SrcComp
                                        , CallArg Ty CTy
                                        )
                                     -> TcM ()
                      check_call_arg (CAExp ty, CAExp _arg, CAExp ty')
                        = unify cloc ty ty'
                      check_call_arg (CAComp ct0, CAComp arg, CAComp cty')
                        = do { ct0' <- ct_base_of arg cty'
                             ; unify_cty0 cloc ct0 ct0'
                             }
                      check_call_arg (_,ca,_)
                        = raiseErrNoVarCtx cloc $
                          vcat [ text "Unexpected call argument"
                               , nest 2 $ ppr ca
                               ]

                      ct_base_of :: SrcComp -> CTy -> TcM CTy0
                      ct_base_of _ (CTBase ctb)
                        = return ctb
                      ct_base_of arg _other
                        = raiseErrNoVarCtx cloc $ nonFullAppErr arg

                ; mapM_ check_call_arg (zip3 formal actual actualTys')
                ; return (cCall cloc () f' actualEs', CTBase res)
                }

           Emit _ e ->
             do { (e', ety) <- tyCheckExpr e
                ; ta <- TVar <$> newTyVar "a"
                ; let ty = CTBase (TComp TUnit ta ety)
                ; return (cEmit cloc () ta e', ty)
                }

           Emits _ e ->
             do { (e', ety) <- tyCheckExpr e
                ; ety' <- zonkTy ety
                --; liftIO $ putStrLn $ "tcComp, emits ty = " ++ show ty
                ; case ety' of
                    TArray _ bty ->
                      do { ta <- TVar <$> newTyVar "a"
                         ; let ty = CTBase (TComp TUnit ta bty)
                         ; return (cEmits cloc () ta e', ty)
                         }
                    _ -> raiseErrNoVarCtx cloc (expActualErr unknownTArr ety' e)
                }

           Return _ _ fi e ->
             do { (e', ety) <- tyCheckExpr e
                ; ta <- TVar <$> newTyVar "a"
                ; tb <- TVar <$> newTyVar "b"
                ; let ty = CTBase (TComp ety ta tb)
                ; return (cReturn cloc () ta tb fi e', ty)
                }

           Interleave c1 c2 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; let err_msg (x, xty) (y, yty)
                       = vcat [ text "Interleave expects two transformers"
                              , text "but got:"
                              , nest 2 $ ppr x <+> text "of type" <+> ppr xty
                              , text "and"
                              , nest 2 $ ppr y <+> text "of type" <+> ppr yty
                              ]

                ; case cty1 of
                    CTBase (TTrans a b) ->
                      do { (c2', cty2) <- tyCheckComp c2
                         ; case cty2 of
                             CTBase (TTrans a' b') ->
                                do { unify cloc a a'
                                   ; unify cloc b b'
                                   ; let cTy = CTBase (TTrans a b)
                                   ; return (cInterleave cloc () c1' c2', cTy)
                                   }
                             CTBase (TComp _ _ _)
                                -> raiseErrNoVarCtx cloc
                                     (err_msg (c1', cty1) (c2', cty2))
                             CTArrow _ _
                                -> raiseErrNoVarCtx cloc $ nonFullAppErr c2

                         }
                    CTBase (TComp {}) ->
                      do { (c2', cty2) <- tyCheckComp c2
                         ; raiseErrNoVarCtx cloc
                             (err_msg (c1', cty1) (c2', cty2))
                         }
                    CTArrow {} -> raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           Branch e c1 c2 ->
             do { (e',  tye)  <- tyCheckExpr e
                ; (c1', cty1) <- tyCheckComp c1
                ; (c2', cty2) <- tyCheckComp c2

                ; checkWith cloc (isCTyBase cty1) $ nonFullAppErr c1
                ; checkWith cloc (isCTyBase cty2) $ nonFullAppErr c2

                ; let (CTBase cty1') = cty1
                ; let (CTBase cty2') = cty2

                ; unify_cty0 cloc cty1' cty2'
                ; unify      cloc tye   TBool

                ; return (cBranch cloc () e' c1' c2', cty1)
                }

           Take1 _ _ ->
             do { ta <- TVar <$> newTyVar "a"
                ; tb <- TVar <$> newTyVar "b"
                ; let ty = CTBase (TComp ta ta tb)
                ; return (cTake1 cloc () ta tb, ty)
                }

           Take _ _ n ->
             do { ta <- TVar <$> newTyVar "a"
                ; tb <- TVar <$> newTyVar "b"
                ; let to = TArray (Literal n) ta
                ; let ty = CTBase (TComp to ta tb)
                ; return (cTake cloc () ta tb n, ty)
                }

           Until e c1 ->
             do { (e', t) <- tyCheckExpr e
                ; (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    CTBase (TComp v a b) ->
                      do { unify cloc t TBool
                         ; let cTy = CTBase (TComp v a b)
                         ; return (cUntil cloc () e' c1', cTy)
                         }
                    CTBase (TTrans _ _) ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           While e c1 ->
             do { (e', t) <- tyCheckExpr e
                ; (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    CTBase (TComp v a b) ->
                      do { unify cloc t TBool
                         ; let cTy = CTBase (TComp v a b)
                         ; return (cWhile cloc () e' c1', cTy)
                         }
                    CTBase (TTrans _ _) ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }


           Times ui e elen x c1 ->
             do { (e',    tye)    <- tyCheckExpr e
                ; (elen', tyelen) <- tyCheckExpr elen
                ; (x',    tyx)    <- tyCheckBound x

                ; unifyAll cloc [tye, tyelen, tyx]

                ; (c1', cty1) <- extendEnv [x'] $ tyCheckComp c1
                ; case cty1 of
                    CTBase (TComp v a b) ->
                      do { ti <- newTInt_BWUnknown
                         ; unify cloc tye    ti
                         ; unify cloc tyelen ti -- TODO: This is redundant
                         ; let cTy = CTBase (TComp v a b)
                         ; return (cTimes cloc () ui e' elen' x' c1', cTy)
                         }
                    CTBase (TTrans _ _) ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           Repeat wdth c1 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    CTBase (TComp _v a b) ->
                      -- TODO: Shouldn't we unify v with TUnit here?
                      do { let c1TyNew = CTBase (TTrans a b)
                         ; return (cRepeat cloc () wdth c1', c1TyNew)
                         }
                    CTBase (TTrans _ _) ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           VectComp wdth c1 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    t@(CTBase (TComp {})) ->
                      return (cVectComp cloc () wdth c1', t)
                    CTBase (TTrans _ _) ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           Map wdth f ->
             do { (f', fty) <- tyCheckFree f
                ; ta <- TVar <$> newTyVar "a"
                ; tb <- TVar <$> newTyVar "b"
                ; unify cloc fty (TArrow [ta] tb)
                ; let cTy = CTBase (TTrans ta tb)
                ; return (cMap cloc () wdth f', cTy)
                }

           Filter f ->
             do { (f', fty) <- tyCheckFree f
                ; ta <- TVar <$> newTyVar "a"
                ; unify cloc fty (TArrow [ta] TBool)
                ; let cTy = CTBase (TTrans ta ta)
                ; return (cFilter cloc () f', cTy)
                }

           WriteSnk ann ->
             do { ta <- TVar <$> newTyVar "a"
                ; unifyAnn cloc ann ta
                ; let cty' = CTBase (TTrans ta (TBuff (ExtBuf ta)))
                ; return (cWriteSnk cloc () ta, cty')
                }

           WriteInternal _ bid ->
             do { ta <- TVar <$> newTyVar "a"
                ; let cty' = CTBase (TTrans ta (TBuff (IntBuf ta)))
                ; return (cWriteInternal cloc () ta bid, cty')
                }

           ReadSrc ann ->
             do { ta <- TVar <$> newTyVar "a"
                ; unifyAnn cloc ann ta
                ; let cty' = CTBase (TTrans (TBuff (ExtBuf ta)) ta)
                ; return (cReadSrc cloc () ta, cty')
                }

           ReadInternal _ bid tp ->
             do { ta <- TVar <$> newTyVar "a"
                ; let cty' = CTBase (TTrans (TBuff (IntBuf ta)) ta)
                ; return (cReadInternal cloc () ta bid tp, cty')
                }

           -- Standalone computations (forked onto another core)
           -- must have type [ST T a b]
           Standalone c1 ->
             do { (c1', cty1) <- tyCheckComp c1
                ; case cty1 of
                    CTBase (TTrans _ _) ->
                      return (cStandalone cloc () c1', cty1)
                    CTBase {} ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "transformer" "computer" c1
                    CTArrow {} ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1
                }

           Mitigate _ n1 n2 ->
             do { ta <- TVar <$> newTyVar "a"
                ; let t1 = if n1 == 1 then ta else TArray (Literal n1) ta
                ; let t2 = if n2 == 1 then ta else TArray (Literal n2) ta
                  -- TODO: Check that n1 divides n2 or n2 divides n1
                ; let cty = CTBase (TTrans t1 t2)
                ; return (cMitigate cloc () ta n1 n2, cty)
                }
       }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- Main entry for type checking a computation
tyCheckTopComp :: SrcComp -> TcM Comp
tyCheckTopComp c
  = do { (c_typed, _)  <- tyCheckComp c

         -- TODO: Is this series of three calls really necessary? defaultComp
         -- already _does_ zonking; seems to me that's the only one we need
       ; c_zonked       <- zonkComp c_typed
       ; c_defaulted    <- defaultComp c_zonked
       ; c_final_zonked <- zonkComp c_defaulted

       ; _ <- checkUnresolved c_final_zonked

       ; return c_final_zonked
       }

tyCheckTopDecls :: [(GName (Maybe SrcTy), Maybe SrcExp)]
                -> TcM [(GName Ty, Maybe Exp)]
tyCheckTopDecls decls
  = do { tc_decls <- tyCheckDecls decls
       ; mapLocalsM zonkTy zonkExpr tc_decls
       }

-- TODO: Remove once we get rid of locals
envOfDecls :: [(GName Ty, Maybe Exp)] -> Env
envOfDecls = mkTyEnv . map (\(nm, _) -> (name nm, nameTyp nm))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Translate function formal parameters
--
-- We support type inference (in principle) for expression parameters, although
-- the parser currently insists on type annotations. We do not support it for
-- computation parameters (because we don't have a unification infrastructure
-- for CTy).
tyCheckParam :: GName (CallArg (Maybe SrcTy) (Maybe (GCTy SrcTy)))
             -> TcM (GName (CallArg Ty CTy))
tyCheckParam nm =
    case nameTyp nm of
      CAExp t -> do
        (nm', t') <- tyCheckBound nm{nameTyp = t}
        return nm'{nameTyp = CAExp t'}
      CAComp (Just cty) -> do
        cty' <- trCTy (nameLoc nm) cty
        nm'  <- tyCheckCBound nm{nameTyp = Just cty} cty'
        return nm'{nameTyp = CAComp cty'}
      CAComp Nothing ->
        raiseErrNoVarCtx (nameLoc nm) $
          text "Missing type annotation on parameter" <+> ppr nm

-- | Declarations of local variables
tyCheckDecls :: [(GName (Maybe SrcTy), Maybe SrcExp)]
             -> TcM [(GName Ty, Maybe Exp)]
tyCheckDecls [] = return []
tyCheckDecls ((x,mb):decls)
  = do { (x', ty) <- tyCheckBound x
       ; e' <- ty_check_mb mb ty
       ; rest <- extendEnv [x'] $ tyCheckDecls decls
       ; return $ (x',e') : rest
       }
  where ty_check_mb Nothing _ty
          = return Nothing
        ty_check_mb (Just e) ty
          = do { (e', ty') <- tyCheckExpr e
               ; unify (expLoc e) ty ty'
               ; return (Just e')
               }

-- | Comp fun call arguments
tyCheckCallArg :: CallArg SrcExp SrcComp -> TcM (CallArg Exp Comp, CallArg Ty CTy)
tyCheckCallArg (CAExp e)
  = do { (e', ty) <- tyCheckExpr e
       ; return (CAExp e', CAExp ty)
       }
tyCheckCallArg (CAComp c)
  = do { (c', cty) <- tyCheckComp c
       ; return (CAComp c', CAComp cty)
       }

-- | Function definitions
tyCheckFun :: SrcFun -> TcM Fun
tyCheckFun fn =
  do { let floc = funLoc fn
     ; fn' <- case unFun fn of
         MkFunDefined f params decls ebody ->
           do { (params', paramTys) <- unzip <$> mapM tyCheckBound params
              ; decls' <- extendEnv params' $ tyCheckDecls decls
              ; extendEnv (params' ++ map fst decls') $
                  do { (ebody', retTy) <- tyCheckExpr ebody

                     ; let fty = TArrow paramTys retTy
                     ; let f'  = f{nameTyp = fty}

                       -- The type of ebody may mention the parameters of
                       -- the function, whereas we want it to only mention
                       -- the lengths of the parameters of the function,
                       -- hence it is important to zonk!
                     ; mapFunM zonkTy return zonkExpr $
                         MkFun (MkFunDefined f' params' decls' ebody') floc ()
                     }
             }

         MkFunExternal f params resTy -> do
           do { (params', paramTys) <- unzip <$> mapM tyCheckBound params
                -- Although we don't have a body, the result type may
                -- refer to the arguments so we need to extend the env
              ; extendEnv params' $
                  do { resTy' <- trReqTy "external result" floc resTy
                     ; let fty = TArrow paramTys resTy'
                     ; let f'  = f{nameTyp = fty}
                     ; mapFunM zonkTy return zonkExpr $
                         MkFun (MkFunExternal f' params' resTy') floc ()
                     }
              }

       ; checkUnboundPolymorphism (funLoc fn') (funName fn')
       ; return fn'
       }

checkUnboundPolymorphism :: Maybe SourcePos -> GName Ty -> TcM ()
checkUnboundPolymorphism loc fn
  = do { let TArrow atys rty = nameTyp fn
       ; let lvars = gatherPolyVars atys
             rvars = gatherPolyVars [rty]
       ; checkWith loc (null $ rvars L.\\ lvars) $
         vcat [ text "Function" <+> (text $ show fn)
              , text "Has unresolved return type:" <+> ppr rty
              , text "Parameter rypes:" <+> hsep (map ppr atys)
              ]
       }

-- | Check for unresolved type variables.
--
-- In principle we could just zonk and require that there aren't any, anywhere,
-- but that is problematic, since there may be parts of the code that are not
-- used, and where type inference did not really solve any unification
-- problems. However the "main" part of the code, which is supposed to run
-- should not have any unresolved variables.  Hence, we descend in the context
-- (with findMain, below) and only then we map through the computation with our
-- checker (with comp_combine, below).
checkUnresolved :: Comp -> TcM Comp
checkUnresolved
    = mapCompM return return return return return comp_combine . findMain
  where
    comp_combine c
      | S.null (tyVarsOfCTy (ctComp c))
      = return c
      | otherwise
      = raiseErrNoVarCtx (compLoc c) $
        vcat [ text "Computation:"
             , nest 2 $ ppr c
             , text "has unresolved type:"
             , nest 2 $ ppr (ctComp c)
             ]

    -- Descend the context to find the main computation
    findMain :: Comp -> Comp
    findMain (MkComp (Let _ _         c) _ _) = findMain c
    findMain (MkComp (LetStruct _     c) _ _) = findMain c
    findMain (MkComp (LetE _ _ _      c) _ _) = findMain c
    findMain (MkComp (LetHeader _     c) _ _) = findMain c
    findMain (MkComp (LetFunC _ _ _ _ c) _ _) = findMain c
    findMain (MkComp (LetERef _ _     c) _ _) = findMain c
    findMain other_c                          = other_c

{-------------------------------------------------------------------------------
  Translate types
-------------------------------------------------------------------------------}

trCTy :: Maybe SourcePos -> GCTy SrcTy -> TcM CTy
trCTy p (CTBase cty0)      = CTBase  <$> trCTy0 p cty0
trCTy p (CTArrow args res) = CTArrow <$> mapM (trCA p) args <*> trCTy0 p res

trCTy0 :: Maybe SourcePos -> GCTy0 SrcTy -> TcM CTy0
trCTy0 p (TComp u a b) = TComp  <$> trTy p u <*> trTy p a <*> trTy p b
trCTy0 p (TTrans a b)  = TTrans <$> trTy p a <*> trTy p b

trCA :: Maybe SourcePos -> CallArg SrcTy (GCTy0 SrcTy) -> TcM (CallArg Ty CTy0)
trCA p (CAExp  ty)  = CAExp  <$> trTy   p ty
trCA p (CAComp cty) = CAComp <$> trCTy0 p cty

trStructDef :: Maybe SourcePos -> GStructDef (Maybe SrcTy) -> TcM (GStructDef Ty)
trStructDef p (StructDef nm flds) = do
  flds' <- forM flds $ \(fld, ty) -> do
    ty' <- trReqTy ("struct field " ++ fld) p ty
    return (fld, ty')
  return (StructDef nm flds')
