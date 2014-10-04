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

module TcExpr where

import AstExpr
import AstComp
import TcErrors
import qualified GenSym as GS
import TcMonad

import TcUnify


import qualified Data.Map as M
import qualified Data.List as L
import Data.IORef

import Control.Monad ( unless )

import Text.PrettyPrint.HughesPJ
import PpExpr

import Outputable


tyCheckExprWith :: Exp () -> Ty -> TcM (Exp Ty)
tyCheckExprWith e ty
  = do { e' <- tyCheckExpr e
       ; unify (expLoc e') ty (info e')
       ; return e' }

tyCheckVal :: Val -> TcM Ty
tyCheckVal v
  = case v of
      VBit b    -> return TBit
      VInt n    -> newTInt_BWUnknown
      VDouble d -> return TDouble
      VBool b   -> return TBool
      VString s -> return TString
      VUnit     -> return TUnit

tyCheckExpr :: Exp () -> TcM (Exp Ty)
tyCheckExpr e
  = do { let loc = expLoc e
       ; pushErrCtx (ExprErrCtx e) $
         case unExp e of

          EVal v ->
            do { t <- tyCheckVal v
               ; return (eVal loc t v)
               }

          EValArr a@(h:t) ->
            do { vtys <- mapM tyCheckVal (h:t)
               ; unifyAll loc vtys
               ; let arrty = TArr (Literal (length a)) (head vtys)
               ; return $ eValArr loc arrty a
               }

          EValArr [] ->
            raiseErrNoVarCtx loc $ text ("Empty value array.")


          EVar x ->
            do { env <- getEnv
               ; case M.lookup (name x) env of
                   Just t  -> return $ eVar loc t x
                   Nothing -> raiseErrNoVarCtx loc (text "Unbound variable:" <+> ppName x)
               }

          EUnOp uop e0 ->
            do { e0' <- tyCheckExpr e0
               ; t0  <- zonkTy (info e0')
               ; let msg = text "Unary operator type mismatch:" <+> ppr uop
               ; case uop of
                   Neg ->
                     do { checkWith loc (isScalarTy t0) msg
                        ; return $ eUnOp loc t0 uop e0'
                        }

                   Not ->
                     do { unify loc t0 TBool
                        ; return $ eUnOp loc TBool uop e0'
                        }

                   BwNeg ->
                     do { tint_unknown <- newTInt_BWUnknown
                        ; firstToSucceed (unify loc t0 tint_unknown)
                                         (unify loc t0 TBit)
                        ; return (eUnOp loc t0 uop e0')
                        }

                   Cast target_ty ->
                     do { compat_test <-
                            case (target_ty, t0) of
                              (TBit,TInt _p)             -> return True
                              (TInt _p, TBit)            -> return True
                              (TDouble, TInt _)          -> return True
                              (TInt _p, TDouble)         -> return True
                              (TInt p, TInt p')          -> return True
                              (TStruct _s1, TStruct _s2) -> return $ isComplexTy target_ty && isComplexTy t0
                              -- Otherwise just try to unify
                              (t1,t2) -> unify loc t1 t2 >> return True

                        ; checkWith loc compat_test $
                          text "Invalid cast from type" <+> ppr t0 <+> text "to" <+> ppr target_ty

                        ; return $ eUnOp loc target_ty (Cast target_ty) e0'

                        }

                   ALength
                     | TArr ne ty <- t0 -- NB ne is zonked
                     -> return (eUnOp loc tint ALength e0')
                     | TVar _ <- t0
                     -> raiseErrNoVarCtx loc $
                        text "Could not resolve array length of:" <+> ppr e0
                     | otherwise
                     -> raiseErrNoVarCtx loc (expActualErr unknownTArr t0 e0)

                   NatExp -> error "typeCheckExpr: NatExp not supported!"

               }

          EBinOp bop e1 e2 ->
            do e1' <- tyCheckExpr e1
               e2' <- tyCheckExpr e2
               t1  <- zonkTy (info e1')
               t2  <- zonkTy (info e2')

               let msg = text "Binary operator type mismatch:" <+> ppr bop
               case bop of
                 x | isArithBinOp x -- Add / Sub / Mult / Div / Rem / Expon
                   -> do { -- liftIO $ putStrLn $ "t1 = " ++ show t1
                         ; checkWith loc (supportsArithTy t1) msg
                         ; unify loc t1 t2
                         ; return $ eBinOp loc t1 bop e1' e2'
                         }

                   | isShiftBinOp x -- ShL / ShR
                   -> do { ti1 <- newTInt_BWUnknown
                         ; ti2 <- newTInt_BWUnknown
                         ; unify loc t1 ti1
                         ; unify loc t2 ti2
                         ; return $ eBinOp loc ti1 bop e1' e2'
                         }

                   | isLogicalBinOp x -- BwAnd / BwOr / BwXor (valid for either int or bit)
                   -> do { ti <- newTInt_BWUnknown
                         ; firstToSucceed (unifyMany loc [t1,t1] [ti,t2])
                                          (unifyMany loc [t1,t1] [TBit,t2])
                         ; return $ eBinOp loc t1 bop e1' e2'
                         }

                   | isEqualityBinOp x -- Eq / Neq
                   -> do { checkWith loc (supportsEqTy t1) msg
                         ; unify loc t1 t2
                         ; return $ eBinOp loc TBool bop e1' e2'
                         }

                   | isRelBinOp x -- Let / Leq / Gt / Geq
                   -> do { checkWith loc (supportsCmpTy t1) msg
                         ; unify loc t1 t2
                         ; return $ eBinOp loc TBool bop e1' e2'
                         }

                   | isBoolBinOp x -- And / Or
                   -> do { unify loc t1 TBool
                         ; unify loc t2 TBool
                         ; return $ eBinOp loc TBool bop e1' e2'
                         }
                   | otherwise
                   -> error $ "BUG: Forgotten case for operator " ++ show bop ++ " in typeCheckExpr!"

          EAssign e1 e2 ->
            do { e1' <- tyCheckExpr e1
               ; e2' <- tyCheckExpr e2
               ; unify (expLoc e1') (info e1') (info e2')
               ; return (eAssign loc TUnit e1' e2')
               }

          EArrRead earr eix r ->
            do { earr' <- tyCheckExpr earr
               ; eix'  <- tyCheckExpr eix
               ; t1    <- zonkTy $ info earr'
               ; t2    <- zonkTy $ info eix'
               ; case t1 of
                  TArr n t -> -- TODO: support metavars for array size here
                   case r of
                     LISingleton
                       -> do { ti <- newTInt_BWUnknown
                             ; unify loc t2 ti
                             ; return (eArrRead loc t earr' eix' LISingleton)
                             }

                     LILength r
                       -> do { ti <- newTInt_BWUnknown
                             ; unify loc t2 ti
                             ; return (eArrRead loc (TArr (Literal r) t) earr' eix' (LILength r))
                             }

                  _ -> raiseErrNoVarCtx loc (expActualErr unknownTArr t1 earr')
               }

          EBPerm earr eperm ->
           do { earr' <- tyCheckExpr earr
              ; eix'  <- tyCheckExpr eperm
              ; t1 <- zonkTy (info earr')
              ; t2 <- zonkTy (info eix' )
              ; case t1 of
                  TArr n TBit ->
                    do { ti <- newTInt_BWUnknown
                       ; unify loc t2 (TArr n ti)
                       ; return $ eBPerm loc t1 earr' eix'
                       }
                  _ -> raiseErrNoVarCtx loc $
                       expActualErr (unknownTArrOfBase TBit) t1 earr'
              }

          EArrWrite earr eix r eval ->
           do { earr'  <- tyCheckExpr earr
              ; eval' <- tyCheckExpr eval
              ; eix'  <- newTInt_BWUnknown >>= tyCheckExprWith eix
              ; tarr  <- zonkTy (info earr')
              ; tval  <- zonkTy (info eval')
              ; case r of
                 LISingleton
                    -> case tarr of
                         TArr n tarrt ->
                           do { unify loc tarrt tval
                              ; return (eArrWrite loc TUnit earr' eix' r eval')
                              }
                         _ -> raiseErrNoVarCtx loc $
                              expActualErr unknownTArr tarr earr'

                 LILength l
                    -> case tarr of
                         TArr n tarrt
                           | Literal nn <- n
                           , nn < l
                           -> raiseErrNoVarCtx loc $
                              vcat [ text "Array write operation interval " <+> int l
                                   , text "greater than array size" <+> int nn ]
                           | otherwise
                           -> do { unify loc (TArr (Literal l) tarrt) tval
                                 ; return $
                                   eArrWrite loc TUnit earr' eix' r eval'
                                 }
                         _ ->  raiseErrNoVarCtx loc $
                               expActualErr unknownTArr tarr earr'
              }

          EFor ui ix estart elen ebody ->
            do estart' <- tyCheckExpr estart
               elen'   <- tyCheckExpr elen
               unify loc (info estart') (info elen')

               ixType <- case mbtype ix of
                 Just t  -> do unify loc t (info estart') ; return t
                 Nothing -> return (info estart')

               ebody'  <- extendEnv [(name ix, ixType)] (tyCheckExpr ebody)
               unify loc (info ebody') TUnit
               return $ eFor loc (info ebody') ui ix estart' elen' ebody'


          EWhile econd ebody ->
            do econd' <- tyCheckExpr econd
               ebody' <- tyCheckExpr ebody
               unify loc (info econd') TBool
               unify loc (info ebody') TUnit
               return $ eWhile loc (info ebody') econd' ebody'

          EIter ix x earr ebody ->
            do { earr' <- tyCheckExpr earr
               ; tarr  <- zonkTy (info earr')
               ; case tarr of
                   TArr n tbase ->
                      do { ebody' <- extendEnv [ (name ix, maybe tint id (mbtype ix))
                                               , (name x,tbase)
                                               ] $ tyCheckExpr ebody
                         ; unify loc (info ebody') TUnit
                         ; return $ eIter loc (info ebody') ix x earr' ebody'
                         }
                   _ -> raiseErrNoVarCtx loc $
                        expActualErr unknownTArr tarr earr'
              }

          ELet x fi e1 e2 ->
            do { e1' <- tyCheckExpr e1
               ; let t1 = info e1'
               ; e2' <- extendEnv [(name x,t1)] $ tyCheckExpr e2
               ; return $ eLet loc (info e2') x fi e1' e2'
               }

          ELetRef x tyAnn (Just e1) e2 ->
            do { e1' <- tyCheckExpr e1
               ; unify loc tyAnn (info e1')
               ; let t1 = info e1'
               ; e2' <- extendEnv [(name x,t1)] $ tyCheckExpr e2
               ; return $ eLetRef loc (info e2') x tyAnn (Just e1') e2'
               }

          ELetRef x t1 Nothing e2 ->
            do { e2' <- extendEnv [(name x,t1)] $ tyCheckExpr e2
               ; return $ eLetRef loc (info e2') x t1 Nothing e2'
               }

          ESeq e1 e2 ->
            do { e1' <- tyCheckExpr e1
               ; e2' <- tyCheckExpr e2
               ; return $ eSeq loc (info e2') e1' e2'
               }
          ECall e1 es2 ->
            do { e1'  <- tyCheckExpr e1
               ; es2' <- tyCheckExprs es2
               ; t1   <- zonkTy (info e1')
--               ; liftIO $ putStrLn $ "Before instantiation = " ++ show t1
               ; t1'  <- instantiateCall t1
--               ; liftIO $ putStrLn $ "After instantiation = " ++ show t1'
               ; case t1' of
                   TArrow tas tb ->
                     do { checkWith loc (length tas == length es2) $
                          vcat [ text "Function" <+> ppr e1'
                               , text "Expecting" <+> int (length tas) <+>
                                                      text "arguments"
                               , text "but was given" <+> int (length es2)
                               ]

                          -- BOZIDAR: change below to enable polymorphic arrays
                          -- unify loc $ zip (tyListOfProd ta) (map info es2')

                        ; unifyMany loc tas (map info es2')
                        ; return $ eCall loc tb e1' es2'
                        }
                   _ -> raiseErrNoVarCtx loc $
                        expActualErr (unknownTFun (length es2)) t1 e1'
               }

          EIf be e1 e2 ->
            do { be' <- tyCheckExpr be
               ; e1' <- tyCheckExpr e1
               ; e2' <- tyCheckExpr e2
               ; unify loc (info be') TBool
               ; unify loc (info e1') (info e2')
               ; return $ eIf loc (info e1') be' e1' e2' }

          EPrint nl e1 ->
            do { e1' <- tyCheckExpr e1
               ; return $ ePrint loc TUnit nl e1'
               }

          EError str ->
            do { a <- newTyVar "a"
               ; let ta = TVar a
               ; return $ eError loc ta str
               }
--             ; return $ eError loc TUnit str


          ELUT r e1 ->
            do { e1' <- tyCheckExpr e1
               ; return $ eLUT loc (info e1') r e1'
               }

          EProj e fn ->
            do { e'  <- tyCheckExpr e
               ; ty' <- zonkTy (info e')
               ; case ty' of
                  TStruct nm
                    -> do { struct <- lookupTDefEnv nm loc
                          ; case lookup fn (struct_flds struct) of
                              Nothing -> raiseErrNoVarCtx loc $
                                         text ("Unknown field " ++ fn ++ " projected out of type " ++ nm)
                              Just fty -> return $ eProj loc fty e' fn
                          }
                  _other -> raiseErrNoVarCtx loc $
                            text "Field projection from non-struct type: " <+> ppr ty'
               }

          EStruct tn tfs ->
            do { struct <- lookupTDefEnv tn loc
               ; let tc_field ((f,fe),(f',fty))
                       | f == f'
                       = do { fe' <- tyCheckExpr fe
                            ; unify loc (info fe') fty
                            ; return (f,fe') }
                       | otherwise
                       = raiseErrNoVarCtx loc $
                         text ("Expecting field " ++ f' ++ " but got " ++ f)

               ; tfs' <- mapM tc_field (zip tfs (struct_flds struct))
               ; return $ eStruct loc (TStruct tn) tn tfs'
               }
       }

tyCheckExprs :: [Exp ()] -> TcM [Exp Ty]
tyCheckExprs = mapM tyCheckExpr


tyBindingsOfDecls :: [(Name,Ty,Maybe (Exp a))] -> [(String,Ty)]
tyBindingsOfDecls = map (\(nm,ty,_) -> (name nm, ty))

envOfDecls :: [(Name,Ty,Maybe (Exp a))] -> Env
envOfDecls = mkTyEnv . tyBindingsOfDecls

tyBindingsOfParams :: [(Name,Ty)] -> [(String,Ty)]
-- Make sure that parameters with polymorphic array sizes
-- introduce the length variables too!
tyBindingsOfParams prms
  = L.nub $ concat $ map param_binds prms
  where param_binds (nm, ty@(TArr (NVar nm' _) _))
          = [(name nm,ty),(name nm',tint)]
        param_binds (nm, ty)
          = [(name nm, ty)]


tyEnvOfParams :: [(Name,Ty)] -> TyEnv
tyEnvOfParams = mkTyEnv . tyBindingsOfParams

tyOfParams :: [(Name,Ty)] -> [Ty]
tyOfParams params = map snd params


tyCheckDecls :: [(Name, Ty, Maybe (Exp ()))]
             -> TcM [(Name, Ty, Maybe (Exp Ty))]
tyCheckDecls [] = return []
tyCheckDecls ((x,ty,mb):decls)
  = do { e' <- ty_check_mb mb ty
       ; rest <- extendEnv [(name x,ty)] $
                 tyCheckDecls decls
       ; return $ (x,ty,e') : rest
       }
  where ty_check_mb Nothing ty
          = return Nothing
        ty_check_mb (Just e) ty
          = do { e' <- tyCheckExprWith e ty
               ; return (Just e')
               }

tyCheckFun :: Fun () -> TcM (Fun Ty)
tyCheckFun fn =
  do { fn' <-
          case unFun fn of
            MkFunDefined f params decls ebody ->
              do { decls' <- extendEnv (tyBindingsOfParams params) $
                             tyCheckDecls decls
                 ; let bnds = tyBindingsOfParams params ++ tyBindingsOfDecls decls'
                 ; extendEnv bnds $
                          do { ebody' <- tyCheckExpr ebody

                               -- The type of ebody may mention the parameters
                               -- of the function, whereas we want it to only
                               -- mention the lengths of the parameters of the
                               -- function, hence it is important to zonk!

                             ; let retTy = info ebody'
                             ; let fun = MkFun (MkFunDefined f params decls' ebody')
                                               (funLoc fn)
                                               (TArrow (tyOfParams params) retTy)
                             ; mapFunAndTysM zonkTy zonkTy zonkExpr fun
                             }
                }
            MkFunExternal _f params _ty ->
               extendEnv (tyBindingsOfParams params) $
               do { let fun = MkFun (MkFunExternal _f params _ty) (funLoc fn)
                                    (TArrow (tyOfParams params) _ty)
                  ; f@(MkFun _ _ zty) <- mapFunAndTysM zonkTy zonkTy zonkExpr fun
                  -- ; liftIO $ putStrLn ("Zonked = " ++ show zty)
                  ; return f
                  }

       ; check_unbound_polymorphism (funLoc fn') (funName fn') (funInfo fn')
       ; return fn'
       }

check_unbound_polymorphism loc fn (TArrow atys rty)
  = do { let lvars = gatherPolyVars atys
             rvars = gatherPolyVars [rty]
       ; checkWith loc (null $ rvars L.\\ lvars) $
         vcat [ text "Function" <+> (text $ show fn)
              , text "Has unresolved return type:" <+> ppr rty
              , text "Parameter rypes:" <+> hsep (map ppr atys)
              ]
       }
check_unbound_polymorphism loc fn _other
  = error "Can't happen!"




