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

module TcUnify where

import qualified Data.Map as M
import Data.IORef

import AstExpr
import AstComp
import TcErrors
import qualified GenSym as GS

import Text.Parsec.Pos

import TcMonad

import Text.PrettyPrint.HughesPJ

import PpExpr
import PpComp
import Outputable

import qualified Data.Set as S

import Data.List ( nub )


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
    go (TStruct n1) (TStruct n2)
      | n1 == n2  = return ()
      | otherwise = unifyErr p tya tyb
    go TBool TBool = return ()
    go (TArr n ty1) (TArr m ty2)
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


unifyAll p []         = return ()
unifyAll p [t]        = return ()
unifyAll p (t1:t2:ts) = unify p t1 t2 >> unifyAll p (t2:ts)

unifyMany p t1s t2s = mapM (\(t1,t2) -> unify p t1 t2) (zip t1s t2s)



unifyBitWidth :: Maybe SourcePos -> Ty -> Ty -> BitWidth -> BitWidth -> TcM ()
unifyBitWidth p orig_ty1 orig_ty2 bw1 bw2 = go bw1 bw2
  where go (BWUnknown bvar) bw
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
unifyALen p orig_ty1 orig_ty2 nm1 nm2 = go nm1 nm2
  where
    go (NVar n _m) nm2
      = do { alenv <- getALenEnv
           ; case M.lookup n alenv of
               Just nm1 -> go nm1 nm2
               Nothing  -> goNVar n nm2
           }

    go nm1 (NVar n _m)
      = go (NVar n _m) nm1

    go (Literal i) (Literal j)
      | i == j
      = return ()
      | otherwise
      = unifyErrGeneric (text "Array length mismatch") p orig_ty1 orig_ty2

    -- Invariant: num expression is never an array
    goNVar nvar1 (Literal i)
      = updALenEnv [(nvar1,Literal i)]

    goNVar nvar1 (NVar nvar2 _m)
      | nvar1 == nvar2 = return ()
      | otherwise
      = do { alenv <- getALenEnv
           ; case M.lookup nvar2 alenv of
               Just nm2 ->
                 -- NB: not goNVar
                 go (NVar nvar1 undefined) nm2
               Nothing ->
                 updALenEnv [(nvar1,(NVar nvar2 _m))] }


defaultExpr = mapExpM_aux zonkTy zonk_exp
  where zonk_exp :: Exp Ty -> TcM (Exp Ty)
        zonk_exp e
          | EError {} <- unExp e
          = do { zty <- zonkTy (info e)
               ; zty' <-
                   case zty of
                    TVar {}
                      -> do { unify (expLoc e) zty TUnit
                           ; return TUnit }
                    _ -> return zty
               ; return $ e { info = zty' }
               }
          | otherwise
          = return e

defaultComp :: Comp CTy Ty -> TcM (Comp CTy Ty)
defaultComp = mapCompM_aux zonkTy defaultExpr zonkCTy zonkComp




solveCts :: TcM ()
solveCts = do getStEnv tcm_in_cts  >>= \cs -> pushErrCtx SolvingInCts  (solve_cts "in"  cs)
              getStEnv tcm_out_cts >>= \cs -> pushErrCtx SolvingOutCts (solve_cts "out" cs)
              discardInOutCts
  where solve_cts str cts
         = mapM_ (do_solve str) cts >> unify_all cts
        do_solve str (BaseTyCt p ty tybase)
          = do { sty <- zonkTy ty
               ; stybase <- zonkTy tybase
               ; case sty of
                   TArr _ sty0 -> unify p stybase sty0
                   _           -> unify p stybase sty }
        unify_all []   = return ()
        unify_all [ct] = return ()
        unify_all (ct1:ct2:cts)
          = do { unify (ct_pos ct1) (ct_ty ct1) (ct_ty ct2)
               ; unify_all (ct2:cts) }


instantiateCall :: Ty -> TcM Ty
-- Instantiates the array length variables
-- to fresh variables, to be used in subsequent
-- unifications. Notice that these are only the
-- ones bound by the parameters of the function
instantiateCall t@(TArrow tas tb)
  = do { let lvars = gatherPolyVars (tb:tas)
       ; s <- mapM freshen lvars
       ; mapTyM (subst_len s) t
       }
  where freshen lv
          = do { lv' <- newALenVar (name lv)
               ; return (lv, NVar lv' 0) }

        subst_len s (TArr (NVar n _mx) t) =
             case lookup n s of
               Nothing  -> return (TArr (NVar n _mx) t)
               Just ne' -> return (TArr ne' t)
        subst_len s ty = return ty

instantiateCall other
  = return other



gatherPolyVars :: [Ty] -> [Name]
gatherPolyVars tys = nub $ gather tys []
  where
     gather [] acc = acc
     gather ((TArr (NVar nm1 _) _):ts) acc
       = gather ts (nm1:acc)
     gather (t:ts) acc = gather ts acc

