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
{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE CPP #-}
module TcUnify (
    -- * Unification
    Unify
  , unify
  , unifyMany
  , unifyAll
    -- * Check that types have certain shapes
  , Hint(..)
  , unifyTInt
  , unifyTInt'
  , unifyTArray
  , unifyComp
  , unifyTrans
  , unifyCompOrTrans

  , unifyComp_CTy
  , unifyTrans_CTy
  , unifyCompOrTrans_CTy

  , ctyJoin

    -- * Defaulting
  , defaultTy
  , defaultExpr
  , defaultComp
  , defaultProg
    -- * Instantiation
  , instantiateCall
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, forM_)
#else
import Data.Foldable (forM_)
#endif

import Control.Monad hiding (forM_)
import Data.Loc
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

import Utils ( panicStr )

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

unify :: Unify a => SrcLoc -> a -> a -> TcM ()
unify p a b = do
  ctx <- getErrCtx
  pushErrCtx (UnifyErrCtx a b ctx) $ unify' p a b

unifyAll :: Unify a => SrcLoc -> [a] -> TcM ()
unifyAll p = go
  where
    go []         = return ()
    go [_]        = return ()
    go (t1:t2:ts) = unify p t1 t2 >> go (t2:ts)

unifyMany :: Unify a => SrcLoc -> [a] -> [a] -> TcM ()
unifyMany p t1s t2s = mapM_ (\(t1,t2) -> unify p t1 t2) (zip t1s t2s)

{-------------------------------------------------------------------------------
  Unification errors (for internal use)
-------------------------------------------------------------------------------}

unifyErrGeneric :: Zonk a => Doc -> SrcLoc -> a -> a -> TcM x
unifyErrGeneric msg pos ty1 ty2 = do
    zty1 <- zonk ty1
    zty2 <- zonk ty2
    raiseErr True pos $
      vcat [ msg
           , text "Cannot unify" <+> ppr zty1 <+> text "with" <+> ppr zty2
           ]

unifyErr :: Zonk a => SrcLoc -> a -> a -> TcM x
unifyErr = unifyErrGeneric empty

occCheckErr :: Zonk a => SrcLoc -> a -> a -> TcM x
occCheckErr = unifyErrGeneric (text "Occurs check error.")

{-------------------------------------------------------------------------------
  Unification proper
-------------------------------------------------------------------------------}

-- | Unification
class Zonk a => Unify a where
  unify' :: SrcLoc -> a -> a -> TcM ()

-- | Unification of types
--
-- TODO: Should we check something about struct fields?  (TStructs are
-- completed internally, so if the names are equal but the fields are not then
-- this is a compiler bug, not a type error)
instance Unify Ty where
  unify' p = go
    where
      go :: Ty -> Ty -> TcM ()
      go (TVar x) ty = do { tenv <- getTyEnv
                          ; case M.lookup x tenv of
                              Just xty -> go xty ty
                              Nothing  -> goTyVar x ty
                          }
      go ty (TVar x) = go (TVar x) ty

      go TUnit          TUnit          = return ()
      go TBit           TBit           = return ()
      go TDouble        TDouble        = return ()
      go TBool          TBool          = return ()
      go (TInt bw1 sg1) (TInt bw2 sg2) | sg1 == sg2 = unify p bw1 bw2
      go (TInt _ sg1)   (TInt _ sg2)
        = checkWith p (sg1 == sg2) (text "Signedness mismatch")
      go (TArray n ty1) (TArray m ty2) = unify p n m >> go ty1 ty2

      go (TBuff (IntBuf ta)) (TBuff (IntBuf tb)) = go ta tb
      go (TBuff (ExtBuf ta)) (TBuff (ExtBuf tb)) = go ta tb

      go (TInterval n1) (TInterval n2) | n1 == n2 = return ()
      go (TStruct n1 _) (TStruct n2 _) | n1 == n2 = return ()

      go (TArrow args res) (TArrow args' res') | length args == length args'
        = do { mapM_ (uncurry go_arg) (zip args args')
             ; go res res'
             }

      go TVoid TVoid = return ()
      go TVoid _b    = panicStr "Unifier cannot possibly meet TVoid!"
      go _a TVoid    = panicStr "Unifier cannot possibly meet TVoid!"

      go a b = unifyErr p a b

      go_arg (GArgTy t1 m1) (GArgTy t2 m2)
        = do checkWith p (m1 == m2) (text "Mutability annotation mismatch")
             go t1 t2

      goTyVar :: TyVar -> Ty -> TcM ()
      goTyVar x (TVar y)
        | x == y    = return ()
        | otherwise = do { tenv <- getTyEnv
                         ; case M.lookup y tenv of
                             Just yty -> goTyVar x yty
                             Nothing  -> updTyEnv [(x, TVar y)]
                         }
      goTyVar x ty
        | x `S.member` tyVarsOfTy ty
        = occCheckErr p (TVar x) ty
        | TVoid <- ty
        = panicStr "Unifier cannot possibly meet TVoid!"
        | otherwise                  = updTyEnv [(x, ty)]


tyJoin :: SrcLoc -> Ty -> Ty -> TcM Ty
tyJoin _ TVoid ty  = return ty
tyJoin _ ty TVoid  = return ty
tyJoin loc ty1 ty2 = unify loc ty1 ty2 >> return ty1

tyJoinWithHint :: SrcLoc -> Hint Ty -> Ty -> TcM Ty
tyJoinWithHint _loc Infer t     = return t
tyJoinWithHint loc (Check t) t' = tyJoin loc t t'


ctyJoin :: SrcLoc -> CTy -> CTy -> TcM CTy
ctyJoin p = go
  where
      go :: GCTy Ty -> GCTy Ty -> TcM CTy
      go (CTVar x) cty = do { tenv <- getCTyEnv
                            ; case M.lookup x tenv of
                                Just xty -> go xty cty
                                Nothing  -> goTyVar x cty
                            }
      go cty (CTVar x) = go (CTVar x) cty

      go (CTTrans  a b) (CTTrans   a' b')
        = do ax <- tyJoin p a a'
             bx <- tyJoin p b b'
             return $ CTTrans ax bx

      go (CTComp v a b) (CTComp v' a' b')
        = do ax <- tyJoin p a a'
             bx <- tyJoin p b b'
             unify p v v'
             return $ CTComp v ax bx

      go (CTArrow args res) (CTArrow args' res')
        | length args == length args'
        = do argsx <- mapM (uncurry (callArgJoin p)) (zip args args')
             resx  <- go res res'
             return $ CTArrow argsx resx

      go a b = unifyErr p a b

      goTyVar :: CTyVar -> CTy -> TcM CTy
      goTyVar x (CTVar y)
        | x == y    = return (CTVar y)
        | otherwise = do { tenv <- getCTyEnv
                         ; case M.lookup y tenv of
                             Just yty -> goTyVar x yty
                             Nothing  -> do updCTyEnv [(x, CTVar y)]
                                            return (CTVar y)
                         }
      goTyVar x cty
        | x `S.member` snd (tyVarsOfCTy cty)
        = occCheckErr p (CTVar x) cty
        | otherwise
        = updCTyEnv [(x, cty)] >> return cty

callArgJoin :: SrcLoc 
            -> CallArg ArgTy CTy -> CallArg ArgTy CTy 
            -> TcM (CallArg ArgTy CTy)
callArgJoin p (CAExp (GArgTy ty m)) (CAExp (GArgTy ty' m'))
  = do checkWith p (m == m') (text "Call argument mutability mismatch.")
       unify p ty ty'
       return (CAExp (GArgTy ty m))
callArgJoin p (CAComp cty) (CAComp cty') 
  = ctyJoin p cty cty' >>= (return . CAComp)
callArgJoin p a b = unifyErr p a b

{-
instance Unify (GCTy Ty) where
  unify' p = go
    where
      go :: GCTy Ty -> GCTy Ty -> TcM ()
      go (CTVar x) cty = do { tenv <- getCTyEnv
                            ; case M.lookup x tenv of
                                Just xty -> go xty cty
                                Nothing  -> goTyVar x cty
                            }
      go cty (CTVar x) = go (CTVar x) cty

      go (CTTrans  a b) (CTTrans   a' b') = unifyMany p [a, b] [a', b']
      go (CTComp v a b) (CTComp v' a' b') = unifyMany p [v, a, b] [v', a', b']

      go (CTArrow args res) (CTArrow args' res') | length args == length args'
        = do { mapM_ (uncurry (unify p)) (zip args args')
             ; go res res'
             }

      go a b = unifyErr p a b

      goTyVar :: CTyVar -> CTy -> TcM ()
      goTyVar x (CTVar y)
        | x == y    = return ()
        | otherwise = do { tenv <- getCTyEnv
                         ; case M.lookup y tenv of
                             Just yty -> goTyVar x yty
                             Nothing  -> updCTyEnv [(x, CTVar y)]
                         }
      goTyVar x cty
        | x `S.member` snd (tyVarsOfCTy cty) = occCheckErr p (CTVar x) cty
        | otherwise                          = updCTyEnv [(x, cty)]


-}

instance Unify BitWidth where
  unify' p = go
    where
      go :: BitWidth -> BitWidth -> TcM ()
      go (BWUnknown bvar) bw = do { benv <- getBWEnv
                                  ; case M.lookup bvar benv of
                                      Just bw1 -> go bw1 bw
                                      Nothing  -> goBWVar bvar bw
                                  }
      go bw (BWUnknown bvar) = go (BWUnknown bvar) bw

      go b1 b2
        | b1 == b2  = return ()
        | otherwise = raiseErr True p (text "Int width mismatch")

      goBWVar :: BWVar -> BitWidth -> TcM ()
      goBWVar bvar1 (BWUnknown bvar2)
        | bvar1 == bvar2 = return ()
        | otherwise      = do { benv <- getBWEnv
                              ; case M.lookup bvar2 benv of
                                  Just bw -> goBWVar bvar1 bw
                                  Nothing -> updBWEnv [(bvar1, BWUnknown bvar2)]
                              }
      -- Occurs check not necessary
      goBWVar bvar1 bw2 = updBWEnv [(bvar1,bw2)]

instance Unify NumExpr where
  unify' p = go
    where
      go :: NumExpr -> NumExpr -> TcM ()
      go (NVar n) nm2 = do { alenv <- getALenEnv
                           ; case M.lookup n alenv of
                               Just nm1 -> go nm1 nm2
                               Nothing  -> goNVar n nm2
                           }
      go nm1 (NVar n) = go (NVar n) nm1

      go (Literal i) (Literal j)
        | i == j    = return ()
        | otherwise = raiseErr True p (text "Array length mismatch")

      goNVar :: LenVar -> NumExpr -> TcM ()
      goNVar nvar1 (NVar nvar2)
        | nvar1 == nvar2 = return ()
        | otherwise      = do { alenv <- getALenEnv
                              ; case M.lookup nvar2 alenv of
                                  Just nm2 -> goNVar nvar1 nm2
                                  Nothing  -> updALenEnv [(nvar1, NVar nvar2)]
                              }
      -- Occurs check not necessary
      goNVar nvar1 (Literal i) = updALenEnv [(nvar1,Literal i)]

instance (Unify a, Unify b) => Unify (CallArg a b) where
  unify' p = go
    where
      go (CAExp  ty)  (CAExp  ty')  = unify p ty  ty'
      go (CAComp cty) (CAComp cty') = unify p cty cty'
      go a b = unifyErr p a b


{-------------------------------------------------------------------------------
  Defaulting
-------------------------------------------------------------------------------}

-- | @defaultTy p ty def@ defaults @ty@ to @tdef@ if @ty@ is a type variable
-- (after zonking).
--
-- Returns @ty@ if @ty@ is not a type variable and @def@ otherwise.
--
-- Be careful calling this: only call this if you are sure that later
-- unification equations will not instantiate this type variable.
defaultTy :: SrcLoc -> Ty -> Ty -> TcM Ty
defaultTy p ty def = do
    ty' <- zonk ty
    case ty' of
      TVar _ -> do unify p ty' def ; return def
      _      -> return ty'

defaultBitWidths :: Ty -> TcM Ty
defaultBitWidths ty = do
    ty' <- zonk ty
    case ty' of
      TInt (BWUnknown _) Signed   -> do unify noLoc ty' tint32 ; return tint32
      TInt (BWUnknown _) Unsigned -> do unify noLoc ty' tuint32 ; return tuint32
      _                           -> return ty'

-- | Zonk all type variables and default the type of `EError` to `TUnit` when
-- it's still a type variable.
defaultExpr :: Exp -> TcM Exp
defaultExpr = mapExpM defaultBitWidths return goExp
  where
    goExp :: Exp -> TcM Exp
    goExp e
      | EError ty str <- unExp e
      = do { ty' <- defaultTy (expLoc e) ty TUnit
           ; return $ eError (expLoc e) ty' str
           }
      | EVar nm <- unExp e
        -- Extra sanity check:
        -- Make sure every variable has assigned mutability
      = seq (nameMut nm) $ return e
      | otherwise
      = return e

defaultComp :: Comp -> TcM Comp
defaultComp = mapCompM zonk defaultBitWidths return return defaultExpr return

defaultProg :: Prog -> TcM Prog
defaultProg (MkProg comp) = MkProg `liftM` defaultComp comp

{-------------------------------------------------------------------------------
  Instantiation (of polymorphic functions)
-------------------------------------------------------------------------------}

-- | Instantiates the array length variables to fresh variables, to be used in
-- subsequent unifications.
--
-- Notice that these are only the ones bound by the parameters of the function
instantiateCall :: Ty -> TcM Ty
instantiateCall = go
  where
    go :: Ty -> TcM Ty
    go t@(TArrow tas tb) = do let argtys = map argty_ty tas
                                  lvars = gatherPolyVars (tb:argtys)
                              s <- mapM freshen lvars
                              mapTyM (subst_len s) t

    go other             = return other

    freshen :: LenVar -> TcM (LenVar, NumExpr)
    freshen lv
      = do { ne <- freshNumExpr lv
           ; return (lv, ne)
           }

    subst_len :: [(LenVar, NumExpr)] -> Ty -> TcM Ty
    subst_len s (TArray (NVar n) t) =
         case lookup n s of
           Nothing  -> return (TArray (NVar n) t)
           Just ne' -> return (TArray ne' t)
    subst_len _ ty = return ty

{-------------------------------------------------------------------------------
  Check that types have certain shapes

  These functions are a bit more involved than they need to be, because we
  make an effort to try avoid unification variables where possible.
-------------------------------------------------------------------------------}

-- | Type checking hints (isomorphic with Maybe)
data Hint a = Check a | Infer
  deriving (Functor, Foldable)

-- | Analagous to 'maybe'
hint :: b -> (a -> b) -> Hint a -> b
hint _ f (Check a) = f a
hint b _ Infer     = b

unifyTInt :: SrcLoc -> Hint BitWidth -> Ty -> TcM BitWidth
unifyTInt loc annBW = zonk >=> go
  where
    go (TInt bw _) = do
      forM_ annBW $ unify loc bw
      return bw
    go ty = do
      bw <- hint (freshBitWidth "bw") return annBW
      firstToSucceed (unify loc ty (TInt bw Signed))
                     (unify loc ty (TInt bw Unsigned))
      return bw

-- Version of unifyTInt where we don't care about the bitwidths
unifyTInt' :: SrcLoc -> Ty -> TcM ()
unifyTInt' loc ty = void $ unifyTInt loc Infer ty

unifyTArray :: SrcLoc -> Hint NumExpr -> Hint Ty -> Ty -> TcM (NumExpr, Ty)
unifyTArray loc annN annA = zonk >=> go
  where
    go (TArray n a) = do
      forM_ annN $ unify loc n
      forM_ annA $ unify loc a
      return (n, a)
    go ty = do
      n <- hint (freshNumExpr "n") return annN
      a <- hint (freshTy      "a") return annA
      unify loc ty (TArray n a)
      return (n, a)

unifyComp :: SrcLoc
          -> Hint Ty -> Hint Ty -> Hint Ty
          -> CTy -> TcM (Ty, Ty, Ty)
unifyComp loc annU annA annB = zonk >=> go
  where
    go (CTComp u a b) = do
      u' <- tyJoinWithHint loc annU u
      a' <- tyJoinWithHint loc annA a
      b' <- tyJoinWithHint loc annB b
      return (u', a', b')
    go cty = do
      u <- hint (freshTy "u") return annU
      a <- hint (freshTy "a") return annA
      b <- hint (freshTy "b") return annB
      CTComp ux ax bx <- ctyJoin loc cty (CTComp u a b)
      return (ux, ax, bx)


unifyTrans :: SrcLoc
           -> Hint Ty -> Hint Ty
           -> CTy -> TcM (Ty, Ty)
unifyTrans loc annA annB = zonk >=> go
  where
    go (CTTrans a b) = do
      a' <- tyJoinWithHint loc annA a
      b' <- tyJoinWithHint loc annB b
      return (a', b')
    go cty = do
      a <- hint (freshTy "a") return annA
      b <- hint (freshTy "b") return annB
      CTTrans ax bx <- ctyJoin loc cty (CTTrans a b)
      return (ax, bx)

-- | Sadly, we cannot have a unification constraint that somehing must
-- either be a computer or a transformer, so if we find a type variable here
-- we must give a type error.
unifyCompOrTrans :: SrcLoc
                 -> Hint Ty -> Hint Ty
                 -> CTy -> TcM (Maybe Ty, Ty, Ty)
unifyCompOrTrans loc annA annB = zonk >=> go
  where
    go (CTComp u a b) = do
      a' <- tyJoinWithHint loc annA a
      b' <- tyJoinWithHint loc annB b
      return (Just u, a', b')
    go (CTTrans a b) = do
      a' <- tyJoinWithHint loc annA a
      b' <- tyJoinWithHint loc annB b
      return (Nothing, a', b')
    go (CTArrow _ _) =
      raiseErrNoVarCtx loc $
        text "Expected computer or transformer, but got function"
    go (CTVar _) =
      raiseErrNoVarCtx loc $
        text "Expected computer or transformer, but got type variable (bug?)"

unifyCompOrTrans_CTy :: SrcLoc
                 -> Hint Ty -> Hint Ty
                 -> CTy -> TcM CTy
unifyCompOrTrans_CTy loc annA annB cty
  = do (nm,a,b) <- unifyCompOrTrans loc annA annB cty
       case nm of
          Nothing -> return $ CTTrans a b
          Just v  -> return $ CTComp v a b

unifyTrans_CTy :: SrcLoc
              -> Hint Ty -> Hint Ty
              -> CTy -> TcM CTy
unifyTrans_CTy loc annA annB cty
  = do (a,b) <- unifyTrans loc annA annB cty
       return $ CTTrans a b

unifyComp_CTy :: SrcLoc
          -> Hint Ty -> Hint Ty -> Hint Ty
          -> CTy -> TcM CTy
unifyComp_CTy loc annU annA annB cty
  = do (u,a,b) <- unifyComp loc annU annA annB cty
       return $ CTComp u a b

