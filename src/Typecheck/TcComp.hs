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
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module TcComp ( tyCheckTopComp, tyCheckTopDecls ) where

import AstExpr
import AstComp
import TcErrors
import qualified GenSym as GS

import TcMonad
import TcUnify 

import Data.Maybe ( fromJust ) 
import TcExpr

import qualified Data.Map as M
import Data.IORef

import Text.PrettyPrint.HughesPJ
import PpExpr
import PpComp


import qualified Data.Set as S

import Control.Monad ( when ) 

import Eval ( evalInt ) 



tyCheckCompWith :: Comp () () 
                -> Ty -- in ty
                -> Ty -- yld ty
                -> TcM (Comp CTy Ty)
tyCheckCompWith c ta tb
  = do { c' <- tyCheckComp c
       ; let (CTBase cTy0) = compInfo c'
             ta' = inTyOfCTy cTy0
             tb' = yldTyOfCTy cTy0
       ; unify (compLoc c) ta ta'
       ; unify (compLoc c) tb tb'
       ; return c'
       }

tyCheckBinds :: Comp () () -> 
                Comp () () ->  
                [(Name, Comp () ())] -> 
                TcM (Comp CTy Ty)
tyCheckBinds c c1 [] 
  = tyCheckComp c1

tyCheckBinds c c1 ((x,c2):rest)
  = do { c1' <- tyCheckComp c1

       ; let cty1 = compInfo c1' 
    
       ; checkWith (compLoc c1) (hasDoneTyBase cty1) $
         expectedButFound "computer" "transformer" c1' 

       ; let done_ty = fromJust $ doneTyOfCTyBase cty1
 
       ; case mbtype x of
           Nothing -> return ()
           Just ty -> unify (compLoc c1) ty done_ty
       
       ; let ta = inTyOfCTyBase cty1
             tb = yldTyOfCTyBase cty1

       ; rest' <- extendEnv [(name x,done_ty)] $
                  tyCheckCompWith (MkComp (mkBindMany c2 rest) 
                                          (compLoc c)
                                          (compInfo c)) ta tb

       ; return $ MkComp (mkBind c1' (x,rest')) 
                         (compLoc c) (compInfo rest')

       }

tyCheckCallArg :: CallArg (Exp ()) (Comp () ()) 
               -> TcM (CallArg (Exp Ty) (Comp CTy Ty))
tyCheckCallArg (CAExp e)
  = do { e' <- tyCheckExpr e
       ; return (CAExp e') 
       }
tyCheckCallArg (CAComp c) 
  = do { c' <- tyCheckComp c
       ; return (CAComp c') 
       }

tyCheckComp :: Comp () () -> TcM (Comp CTy Ty)
tyCheckComp c 
  = do { let cloc = compLoc c
       ; pushErrCtx (CompErrCtx c) $
         case unComp c of
           Var x ->
             do { cty <- lookupCEnv (name x) cloc 
                ; return $ cVar cloc cty x }

           BindMany c1 xs_cs ->
             tyCheckBinds c c1 xs_cs

           Seq c1 c2 ->
             do { nm <- genSym "_x"
                ; tyCheckComp $ 
                  MkComp (mkBind c1 (toName nm Nothing Nothing, c2)) cloc () 
                }

           Par parInfo c1 c2 ->
             do { c1' <- tyCheckComp c1
                ; let c1loc = compLoc c1'
                      c1ty  = compInfo c1'
                ; case c1ty of
                    CTBase (TTrans a b)  -> 
                        do { c2' <- tyCheckComp c2
                           ; case compInfo c2' of
                               CTBase (TTrans b' t) ->
                                 do { unify cloc b b'
                                    ; let cTy = CTBase (TTrans a t)
                                    ; return $ 
                                      cPar cloc cTy parInfo c1' c2'
                                    }
                               CTBase (TComp u b' t) ->
                                 do { unify cloc b b'
                                    ; let cTy = CTBase (TComp u a t)
                                    ; return $ 
                                      cPar cloc cTy parInfo c1' c2'
                                    }
                               CTArrow _ _ 
                                 -> raiseErrNoVarCtx cloc (nonFullAppErr c2')
                           }
                    CTBase (TComp v a b) -> 
                        do { c2' <- tyCheckComp c2
                           ; case compInfo c2' of
                               CTBase (TTrans b' t) ->
                                 do { unify cloc b b' 
                                    ; let cTy = CTBase (TComp v a t) 
                                    ; return $ cPar cloc cTy parInfo c1' c2'
                                    }
                               CTBase (TComp u b' t) ->
                                 do { cty1 <- zonkCTy (compInfo c1')
                                    ; cty2 <- zonkCTy (compInfo c2') 
                                    ; raiseErr False cloc $
                                      vcat [ text "Computer-Computer (>>>) composition"
                                           , text "Left computer:"
                                           , nest 2 (ppComp c1')
                                           , text "Type:" <+> ppCTy cty1
                                           , text "Right computer:"
                                           , nest 2 (ppComp c2')
                                           , text "Type:" <+> ppCTy cty2
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
                                 -> raiseErrNoVarCtx cloc (nonFullAppErr c2')
                           }
                    CTArrow {} -> raiseErrNoVarCtx cloc (nonFullAppErr c1')
                }


           Let x c1 c2 ->
             do { c1' <- tyCheckComp c1
                ; let t1 = compInfo c1'
                ; c2' <- extendCEnv [(name x,t1)] $ tyCheckComp c2
                ; return $ cLet cloc (compInfo c2') x c1' c2'
                }

           LetStruct sdef c2 ->
             do { c2' <- extendTDefEnv [(struct_name sdef,sdef)] $ 
                         tyCheckComp c2
                ; return $ cLetStruct cloc (compInfo c2') sdef c2'
                }

           LetE x fi e c1 ->
             do { e' <- tyCheckExpr e
                ; let t = info e'
                ; c1' <- extendEnv [(name x,t)] $ tyCheckComp c1 
                ; return $ cLetE cloc (compInfo c1') x fi e' c1'
                }
           
           -- CL
           LetERef x (Right e) c1 ->
             do { e' <- tyCheckExpr e
                ; let t = info e'
                ; c1' <- extendEnv [(name x,t)] $ tyCheckComp c1
                ; return $ cLetERef cloc (compInfo c1') x (Right e') c1'
                }

           LetERef x (Left t) c1 ->
             do { c1' <- extendEnv [(name x,t)] $ tyCheckComp c1
                ; return $ cLetERef cloc (compInfo c1') x (Left t) c1'
                }

           LetHeader x fn c1 ->
             do { fn' <- tyCheckFun fn
                ; let t = funInfo fn'
                -- ; liftIO $ putStrLn $ "Fun type = " ++ show t
                ; c1' <- extendEnv [(name x,t)] $ tyCheckComp c1
                ; return $ cLetHeader cloc (compInfo c1') x fn' c1'
                }
           --

           LetFunC f params locls c1 c2 ->
             do { let from_ty (x, CAExp t)   = [(x,t)]
                      from_ty (x, _)        = []
                      from_cty (x, CAComp t) = [(name x, CTBase t)]
                      from_cty (x, _)       = []
                ; let eparams = concat (map from_ty params)
                      cparams = concat (map from_cty params)

                ; locls' <- extendEnv (tyBindingsOfParams eparams) $ 
                            tyCheckDecls locls

                -- NB: order in which Env is extended is important here, 
                -- for shadowing          

                ; let env = tyBindingsOfParams eparams ++ 
                            tyBindingsOfDecls locls'
                ; c1' <- extendEnv env $ extendCEnv cparams $ tyCheckComp c1

                ; case compInfo c1' of
                    CTBase cty0 ->
                      do { let argtys = map snd params
                         ; c2' <- extendCEnv [(name f, CTArrow argtys cty0)] $ 
                                  tyCheckComp c2
                         ; return $ 
                           cLetFunC cloc (compInfo c2') f params locls' c1' c2'
                         }
                    CTArrow _ _ ->
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }


           Call f es2_pre ->
             do { let es2 = es2_pre
                ; cenv <- getCEnv

                ; fun_ty <- lookupCEnv (name f) cloc
                ; checkWith cloc (not (isCTyBase fun_ty)) $
                  vcat [ text "Computation " <+> ppName f
                       , text "applied, but has non-arrow type:" <+> 
                         ppCTy fun_ty
                       ]
                 
                ; let CTArrow t1s cty = fun_ty 
                ; let fArity = length t1s

                ; checkWith cloc (fArity == length es2) $
                  vcat [ text "Computation function" <+> ppName f
                       , text "Expecting" <+> int fArity <+> text "arguments" 
                       , text "but was given" <+> int (length es2) 
                       ]

                ; es2' <- mapM tyCheckCallArg es2

                ; let check_call_arg (CAExp ty, CAExp e)     
                        = unify cloc ty (info e)
                      check_call_arg (CAComp ct0, CAComp ca) 
                        = do { ctb <- ct_base_of ca (compInfo ca) 
                             ; unify_cty0 cloc ct0 ctb
                             }
                      check_call_arg (_,ca)
                        = raiseErrNoVarCtx cloc $ 
                          vcat [ text "Unexpected call argument"
                               , nest 2 $ ppCallArg ca 
                               ] 

                      ct_base_of ca (CTBase ctb) 
                        = return ctb
                      ct_base_of ca _other 
                        = raiseErrNoVarCtx cloc $ nonFullAppErr ca

                ; mapM check_call_arg (zip t1s es2')
                ; return $ cCall cloc (CTBase cty) f es2'

                }

           Emit e ->
             do { e' <- tyCheckExpr e
                ; a <- newTyVar "a"
                ; let ta = TVar a
                ; let ty = CTBase (TComp TUnit ta (info e'))
                ; return $ cEmit cloc ty e'
                }

           Emits e ->
             do { e' <- tyCheckExpr e
                ; ty <- zonkTy (info e')
                --; liftIO $ putStrLn $ "tcComp, emits ty = " ++ show ty
                ; case ty of
                    TArr _ bty -> 
                      do { a <- newTyVar "a"
                         ; let ta = TVar a
                         ; return $ 
                           cEmits cloc (CTBase (TComp TUnit ta bty)) e'
                         }
                    _ -> raiseErrNoVarCtx cloc (expActualErr unknownTArr ty e')
                }
 
           Return fi e ->
             do { e' <- tyCheckExpr e
                ; a <- newTyVar "a"
                ; b <- newTyVar "b"
                ; let ta = TVar a 
                ; let tb = TVar b
                ; return $ cReturn cloc (CTBase (TComp (info e') ta tb)) fi e' 
                }

           Interleave c1 c2 ->
             do { c1' <- tyCheckComp c1
                ; let err_msg x y 
                       = vcat [ text "Interleave expects two transformers" 
                              , text "but got:"
                              , nest 2 $ ppComp x <+> text "of type" 
                                                  <+> ppCTy (compInfo x)
                              , text "and"
                              , nest 2 $ ppComp y <+> text "of type" 
                                                  <+> ppCTy (compInfo y)
                              ]

                ; case compInfo c1' of
                    CTBase (TTrans a b) ->
                      do { c2' <- tyCheckComp c2
                         ; case compInfo c2' of
                             CTBase (TTrans a' b') ->
                                do { unify cloc a a'
                                   ; unify cloc b b'
                                   ; let cTy = CTBase (TTrans a b)
                                   ; return $ cInterleave cloc cTy c1' c2' 
                                   }
                             CTBase (TComp v a b) 
                                -> raiseErrNoVarCtx cloc (err_msg c1' c2')
                             CTArrow _ _ 
                                -> raiseErrNoVarCtx cloc $ nonFullAppErr c2' 

                         }
                    CTBase (TComp {}) -> 
                      do { c2' <- tyCheckComp c2 
                         ; raiseErrNoVarCtx cloc (err_msg c1' c2') 
                         }
                    CTArrow {} -> raiseErrNoVarCtx cloc $ nonFullAppErr c1'

                }

           Branch e c1 c2 ->
             do { e' <- tyCheckExpr e
                ; c1' <- tyCheckComp c1
                ; c2' <- tyCheckComp c2

                ; checkWith cloc (isCTyBase (compInfo c1')) $ 
                  nonFullAppErr c1'

                ; checkWith cloc (isCTyBase (compInfo c2')) $ 
                  nonFullAppErr c2'

                ; let (CTBase cty1) = compInfo c1'
                ; let (CTBase cty2) = compInfo c2'
                -- ; let a1 = inTyOfCTy cty1
                -- ; let b1 = yldTyOfCTy cty1
                -- ; let a2 = inTyOfCTy cty2
                -- ; let b2 = yldTyOfCTy cty2

                ; unify_cty0 cloc cty1 cty2 
                ; unify cloc (info e') TBool 
                -- ; unify cloc a1 a2 
                -- ; unify cloc b1 b2 
                ; let cTy = compInfo c1' 
                ; return $ cBranch cloc cTy e' c1' c2' 
                }

           Take1 ->
             do { a <- newTyVar "a"
                ; b <- newTyVar "b"
                ; let ta = TVar a
                ; let tb = TVar b
                ; return $ cTake1 cloc (CTBase (TComp ta ta tb))
                }

           Take e ->
             do { let e0 = case evalInt e of 
                             Just i -> eVal (expLoc e) () (VInt i)
                             _ -> e

                ; e' <- tyCheckExpr e0
                ; a <- newTyVar "a"
                ; b <- newTyVar "b"

                ; checkWith cloc (isInt (unExp e0)) $ 
                  text "Expecting integer literal but got:" <+> ppExp e

                ; let ta = TVar a
                ; let tb = TVar b
                ; let to = TArr (Literal (getInt (unExp e0))) ta
                ; return $ cTake cloc (CTBase (TComp to ta tb)) e' 
                }
                where getInt (EVal (VInt n)) = fromInteger n
                      getInt _ = error "BUG (tcComp): getInt, can't happen!"
                      isInt (EVal (VInt _))  = True
                      isInt _                = False

           Until e c1 ->
             do { e' <- tyCheckExpr e
                ; let t = info e'
                ; c1' <- tyCheckComp c1
                ; case compInfo c1' of
                    CTBase (TComp v a b) ->
                      do { unify cloc (info e') TBool 
                         ; let cTy = CTBase (TComp v a b)
                         ; return $ cUntil cloc cTy e' c1'
                         }
                    CTBase (TTrans a b) ->
                      raiseErrNoVarCtx cloc $ 
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }
           While e c1 ->
             do { e' <- tyCheckExpr e
                ; let t = info e'
                ; c1' <- tyCheckComp c1
                ; case compInfo c1' of
                    CTBase (TComp v a b) ->
                      do { unify cloc (info e') TBool 
                         ; let cTy = CTBase (TComp v a b)
                         ; return $ cWhile cloc cTy e' c1'
                         }
                    CTBase (TTrans a b) ->
                      raiseErrNoVarCtx cloc $ 
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }


           Times ui e elen x c1 ->
             do { e' <- tyCheckExpr e
                ; elen' <- tyCheckExpr elen
                ; let t = info e'
                ; c1' <- extendEnv [(name x,tint)] $ tyCheckComp c1
                ; case compInfo c1' of
                    CTBase (TComp v a b) ->
                      do { ti <- newTInt_BWUnknown 
                         ; unify cloc (info e') ti
                         ; unify cloc (info elen') ti
                         ; let cTy = CTBase (TComp v a b)
                         ; return $ cTimes cloc cTy ui e' elen' x c1'
                         }
                    CTBase (TTrans a b) ->
                      raiseErrNoVarCtx cloc $ 
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }

           Repeat wdth c1 ->
             do { c1' <- tyCheckComp c1
                ; case compInfo c1' of
                    CTBase (TComp v a b) ->
                      do { let c1TyNew = CTBase (TTrans a b)
                         ; return $ cRepeat cloc c1TyNew wdth c1'
                         }
                    CTBase (TTrans a b) ->
                      raiseErrNoVarCtx cloc $ 
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }

           VectComp wdth c1 ->
             do { c1' <- tyCheckComp c1
                ; case compInfo c1' of
                    t@(CTBase (TComp {})) ->
                      return $ cVectComp cloc t wdth c1'
                    CTBase (TTrans a b) ->
                      raiseErrNoVarCtx cloc $ 
                      expectedButFound "computer" "transformer" c1
                    CTArrow _ _ -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                } 

           Map wdth nm ->
             do { e' <- tyCheckExpr (eVar cloc () nm)
                ; a <- newTyVar "a"
                ; b <- newTyVar "b"
                ; let ta = TVar a
                ; let tb = TVar b
                ; let t = info e' 
                ; unify cloc t (TArrow [ta] tb)
                ; let cTy = CTBase (TTrans ta tb)
                ; return $ cMap cloc cTy wdth nm
                }

           Filter e ->
             do { e' <- tyCheckExpr e
                ; let t = info e'
                ; a <- newTyVar "a"
                ; let ta = TVar a
                ; unify cloc t (TArrow [ta] TBool)
                ; let cTy = CTBase (TTrans ta TBool)              
                ; return $ cFilter cloc cTy e'
                }

           WriteSnk mty ->
             do { ta <- mk_rw_ty mty 
                ; tabase <- mk_rw_basety mty
                ; let cty' = CTBase (TTrans ta (TBuff (ExtBuf tabase)))
                -- Emit constraint, see Note [IO Type constraints] in TcMonad 
                ; emitOutCt cloc ta tabase
                ; return $ cWriteSnk cloc cty' mty 
                }

           WriteInternal bid -> 
             do { a <- newTyVar "a"
                ; let ta = TVar a
                      cty' = CTBase (TTrans ta (TBuff (IntBuf ta)))
                ; return $ cWriteInternal cloc cty' bid 
                }

           -- Ditto for read    
           ReadSrc mty -> 
             do { ta <- mk_rw_ty mty 
                ; tabase <- mk_rw_basety mty
                ; let cty' = CTBase (TTrans (TBuff (ExtBuf tabase)) ta)
                ; emitInCt cloc ta tabase
                ; return $ cReadSrc cloc cty' mty 
                }

           ReadInternal bid tp -> 
             do { a <- newTyVar "a"
                ; let ta = TVar a 
                      cty' = CTBase (TTrans (TBuff (IntBuf ta)) ta)
                ; return $ cReadInternal cloc cty' bid tp
                }

           -- Standalone computations (forked onto another core) 
           -- must have type [ST T a b]
           Standalone c1 ->
             do { c1' <- tyCheckComp c1
                ; case compInfo c1' of
                    CTBase (TTrans ta tb) -> 
                      return $ cStandalone cloc (compInfo c1') c1'
                    CTBase {} ->
                      raiseErrNoVarCtx cloc $
                      expectedButFound "transformer" "computer" c1' 
                    CTArrow {} -> 
                      raiseErrNoVarCtx cloc $ nonFullAppErr c1'
                }
           Mitigate t n1 n2 -> 
             do { let t1 = if n1 == 1 then t else TArr (Literal n1) t
                ; let t2 = if n2 == 1 then t else TArr (Literal n2) t
                ; let cty = CTBase (TTrans t1 t2)
                ; return $ cMitigate cloc cty t n1 n2
                }
           ActivateTask t mname ->
             failTcM $ text "BUG: ActivateTask should not appear pre-type checking!"
           DeactivateSelf ->
             failTcM $ text "BUG: DeactivateSelf should not appear pre-type checking!"
       }

mk_rw_ty (RWRealTyAnn t) = return t
mk_rw_ty _ = genSym "a" >>= (return . TVar)

mk_rw_basety (RWBaseTyAnn t) = return t
mk_rw_basety _ = genSym "abase" >>= (return . TVar)


seqToBindAux :: Comp a b -> TcM (Comp a b)
seqToBindAux c
  | Seq c1 c2 <- unComp c
  , let cloc = compLoc c
  , let cty  = compInfo c 
  = do { nm <- genSym "__unused_"
       ; return $ MkComp (mkBind c1 (toName nm Nothing Nothing,c2)) cloc cty }
  | otherwise 
  = return c

seqToBind = mapCompM_ return seqToBindAux

-- Main entry for type checking a computation
tyCheckTopComp c 
  = do { c_no_seq <- seqToBind c
       ; c_typed <- tyCheckComp c_no_seq
       ; solveCts 
       ; c_zonked <- zonkComp c_typed
 
         -- default
       ; c_defaulted <- defaultComp c_zonked

         -- zonk out again
       ; c_final_zonked <- zonkComp c_defaulted

       ; checkUnresolved c_final_zonked

       ; return c_final_zonked
       }

tyCheckTopDecls decls 
  = do { tc_decls <- tyCheckDecls decls
       ; zonked_decls <- mapLocalsM zonkExpr tc_decls
       ; return zonked_decls
       }


checkUnresolved :: Comp CTy Ty -> TcM (Comp CTy Ty)
-- Checks for unresolved type variables. In principle we could just
-- zonk and require that there aren't any, anywhere, but that is
-- problematic, since there may be parts of the code that are not
-- used, and where type inference did not really solve any unification
-- problems. However the "main" part of the code, which is supposed to
-- run should not have any unresolved variables.  Hence, we descend in
-- the context (with find_main, below) and only then we map through
-- the computation with our checker (with comp_combine, below).
checkUnresolved c 
  = mapCompM_aux return return return comp_combine (find_main c)
  where 
    comp_combine c@(MkComp c0 loc cty) 
      | S.null (tyVarsOfCTy cty)
      = return c
      | otherwise 
      = raiseErrNoVarCtx loc $ 
        vcat [ text "Computation:"
             , nest 2 $ ppComp c
             , text "has unresolved type:"
             , nest 2 $ ppCTy cty
             ]

    -- Descend the context to find the main computation
    find_main = go
    go (MkComp (Let _ _ c)         _ _) = go c
    go (MkComp (LetStruct _ c)     _ _) = go c
    go (MkComp (LetE _ _ _ c)      _ _) = go c
    go (MkComp (LetHeader _ _ c)      _ _) = go c
    go (MkComp (LetFunC _ _ _ _ c) _ _) = go c
    go other_c = other_c
