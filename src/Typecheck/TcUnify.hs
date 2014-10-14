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
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module TcUnify where

import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as M
import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled
import Outputable
import PpComp ()
import PpExpr ()
import TcMonad

{- Unification
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

unifyErrGeneric :: Doc -> Maybe SourcePos -> Ty -> Ty -> TcM ()
unifyErrGeneric msg pos ty1 ty2
  = do { zty1 <- zonkTy ty1
       ; zty2 <- zonkTy ty2
       ; raiseErr True pos $
         vcat [ msg
              , text "Cannot unify type" <+>
                ppr zty1 <+> text "with" <+> ppr zty2
              ] }

unifyErr :: Maybe SourcePos -> Ty -> Ty -> TcM ()
unifyErr = unifyErrGeneric empty

occCheckErr :: Maybe SourcePos -> Ty -> Ty -> TcM ()
occCheckErr p = unifyErrGeneric (text "Occurs check error.") p


unify_cty0 :: Maybe SourcePos -> CTy0 -> CTy0 -> TcM ()
unify_cty0 p (TTrans a b) (TTrans a' b')
  = do { unify p a a'
       ; unify p b b'
       }
unify_cty0 p (TComp v a b) (TComp v' a' b')
  = do { unify p v v'
       ; unify p a a'
       ; unify p b b'
       }
unify_cty0 p cty0 cty1
  = do { zty1 <- zonkCTy (CTBase cty0)
       ; zty2 <- zonkCTy (CTBase cty1)
       ; raiseErr True p $
         vcat [ text "Cannot unify type" <+>
                ppr zty1 <+> text "with" <+> ppr zty2
              ]
       }


unify :: Maybe SourcePos -> Ty -> Ty -> TcM ()
unify p tya tyb = go tya tyb
  where
    go (TVar x) ty
       = do { tenv <- getTyEnv
            ; case M.lookup x tenv of
                Just xty -> go xty ty
                Nothing  -> goTyVar x ty }
    go ty (TVar x) = go (TVar x) ty
    go TUnit TUnit = return ()
    go TBit TBit   = return ()
    go (TInt bw1) (TInt bw2) = unifyBitWidth p tya tyb bw1 bw2
    go TDouble TDouble = return ()
    go (TBuff (IntBuf ta))(TBuff (IntBuf tb))    = go ta tb
    go (TBuff (ExtBuf bta)) (TBuff (ExtBuf btb)) = go bta btb
    go (TInterval n)(TInterval n')
      | n == n'   = return ()
      | otherwise = unifyErr p tya tyb
    -- TODO: Should we check something about the fields?
    -- (TStructs are completed internally, so if the names are equal but the
    -- fields are not then this is a compiler bug, not a type error)
    go (TStruct n1 _) (TStruct n2 _)
      | n1 == n2  = return ()
      | otherwise = unifyErr p tya tyb
    go TBool TBool = return ()
    go (TArray n ty1) (TArray m ty2)
      = unifyALen p tya tyb n m >> go ty1 ty2
    go (TArrow tys1 ty2) (TArrow tys1' ty2')
      | length tys1 /= length tys1'
      = unifyErr p tya tyb
      | otherwise
      = goMany tys1 tys1' >> go ty2 ty2'

    go _ _ = unifyErr p tya tyb

    goMany ts1 ts2
      = mapM (\(t1,t2) -> go t1 t2) (zip ts1 ts2)

    goTyVar x (TVar y)
      | x == y = return ()
      | otherwise
      = do { tenv <- getTyEnv
           ; case M.lookup y tenv of
               Just yty -> goTyVar x yty
               Nothing  -> updTyEnv [(x,(TVar y))]
           }

    goTyVar x ty
      | x `S.member` tyVarsOfTy ty
      = occCheckErr p tya tyb
      | otherwise
      = updTyEnv [(x,ty)]


unifyAll :: Maybe SourcePos -> [Ty] -> TcM ()
unifyAll p = go
  where
    go []         = return ()
    go [_]        = return ()
    go (t1:t2:ts) = unify p t1 t2 >> go (t2:ts)

unifyMany :: Maybe SourcePos -> [Ty] -> [Ty] -> TcM ()
unifyMany p t1s t2s = mapM_ (\(t1,t2) -> unify p t1 t2) (zip t1s t2s)



unifyBitWidth :: Maybe SourcePos -> Ty -> Ty -> BitWidth -> BitWidth -> TcM ()
unifyBitWidth p orig_ty1 orig_ty2 = go
  where
    go (BWUnknown bvar) bw
      = do { benv <- getBWEnv
           ; case M.lookup bvar benv of
               Just bw1 -> go bw1 bw
               Nothing  -> goBWVar bvar bw }
    go bw (BWUnknown bvar)
      = go (BWUnknown bvar) bw

    go b1 b2
      | b1 == b2
      = return ()
      | otherwise
      = unifyErrGeneric (text "Int width mismatch") p orig_ty1 orig_ty2

    goBWVar bvar1 (BWUnknown bvar2)
      | bvar1 == bvar2
      = return ()
      | otherwise
      = do { benv <- getBWEnv
           ; case M.lookup bvar2 benv of
               Just bw -> goBWVar bvar1 bw
               Nothing -> updBWEnv [(bvar1,BWUnknown bvar2)] }

    goBWVar bvar1 bw2
      = updBWEnv [(bvar1,bw2)]


unifyALen :: Maybe SourcePos -> Ty -> Ty -> NumExpr -> NumExpr -> TcM ()
unifyALen p orig_ty1 orig_ty2 = go
  where
    go (NVar n) nm2
      = do { alenv <- getALenEnv
           ; case M.lookup n alenv of
               Just nm1 -> go nm1 nm2
               Nothing  -> goNVar n nm2
           }

    go nm1 (NVar n)
      = go (NVar n) nm1

    go (Literal i) (Literal j)
      | i == j
      = return ()
      | otherwise
      = unifyErrGeneric (text "Array length mismatch") p orig_ty1 orig_ty2

    -- Invariant: num expression is never an array
    goNVar nvar1 (Literal i)
      = updALenEnv [(nvar1,Literal i)]

    goNVar nvar1 (NVar nvar2)
      | nvar1 == nvar2 = return ()
      | otherwise
      = do { alenv <- getALenEnv
           ; case M.lookup nvar2 alenv of
               Just nm2 ->
                 -- NB: not goNVar
                 go (NVar nvar1) nm2
               Nothing ->
                 updALenEnv [(nvar1, NVar nvar2)]
           }

-- | @defaultTy p ty def@ defaults @ty@ to @tdef@ if @ty@ is a type variable
-- (after zonking).
--
-- Returns @ty@ if @ty@ is not a type variable and @def@ otherwise.
--
-- Be careful calling this: only call this if you are sure that later
-- unification equations will not instantiate this type variable.
defaultTy :: Maybe SourcePos -> Ty -> Ty -> TcM Ty
defaultTy p ty def = do
  ty' <- zonkTy ty
  case ty' of
    TVar _ -> do unify p ty' def ; return def
    _      -> return ty'

-- | Zonk all type variables and default the type of `EError` to `TUnit` when
-- it's still a type variable.
defaultExpr :: Exp -> TcM Exp
defaultExpr = mapExpM zonkTy return zonk_exp
  where
    zonk_exp :: Exp -> TcM Exp
    zonk_exp e
      | EError ty str <- unExp e
      = do { ty' <- defaultTy (expLoc e) ty TUnit
           ; return $ eError (expLoc e) ty' str
           }
      | otherwise
      = return e

defaultComp :: Comp -> TcM Comp
defaultComp = mapCompM zonkCTy zonkTy return return defaultExpr return

-- | Instantiates the array length variables to fresh variables, to be used in
-- subsequent unifications.
--
-- Notice that these are only the ones bound by the parameters of the function
instantiateCall :: Ty -> TcM Ty
instantiateCall = go
  where
    go :: Ty -> TcM Ty
    go t@(TArrow tas tb) = do { let lvars = gatherPolyVars (tb:tas)
                              ; s <- mapM freshen lvars
                              ; mapTyM (subst_len s) t
                              }
    go other             = return other

    freshen :: LenVar -> TcM (LenVar, NumExpr)
    freshen lv
      = do { lv' <- newALenVar lv
           ; return (lv, NVar lv')
           }

    subst_len :: [(LenVar, NumExpr)] -> Ty -> TcM Ty
    subst_len s (TArray (NVar n) t) =
         case lookup n s of
           Nothing  -> return (TArray (NVar n) t)
           Just ne' -> return (TArray ne' t)
    subst_len _ ty = return ty
