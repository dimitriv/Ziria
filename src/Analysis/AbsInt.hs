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
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , ScopedTypeVariables, FunctionalDependencies, FlexibleInstances
  , FlexibleContexts, UndecidableInstances
  , ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
-- | Abstract interpreter

module AbsInt (

   ValDom    (..)
 , CmdDom    (..)
 , CmdDomRec (..)
 , AbsInt    (..)
 , AbsT      (..)
 , POrd      (..)
 , absEval 
 , absEvalRVal
 , inCurrSt
) where


import AstExpr
import AstUnlabelled
import PpExpr ()
import Control.Monad.State.Class
import Control.Applicative
import Data.Loc
import Data.Set ( Set )
import qualified Data.Set as Set
import NameEnv
import CtExpr ( ctExp )

-- import Text.PrettyPrint.HughesPJ
-- import Outputable 
-- import Control.Monad ( unless )
-- import Utils


-- | Variable lvalues 
varLVal :: EId -> LVal v
varLVal = GDVar


{---------------------------------------------------------------------------
  Some infrastructure on partial orders, sets and maps
---------------------------------------------------------------------------}

-- | Partially ordered values
class POrd v where
  pleq :: v -> v -> Bool

-- | Default instances for name environments
pleqNE :: POrd a => NameEnv t a -> NameEnv t a -> Bool
pleqNE m1 m2 = all is_ok (neToList m1)
  where is_ok (nm1,s1) = case neLookup nm1 m2 of
                            Nothing -> False
                            Just s2 -> s1 `pleq` s2

-- | Instances for name environments and sets
instance POrd a => POrd (NameEnv t a) where pleq = pleqNE
instance Ord s => POrd (Set s) where pleq = Set.isSubsetOf


{---------------------------------------------------------------------------
  Type classes for the abstract domain
---------------------------------------------------------------------------}

-- | Value domain
class ValDom v where 
  aVal      :: Val -> v
  aArr      :: [v] -> v
  aUnOp     :: UnOp -> v -> v
  aBinOp    :: BinOp -> v -> v -> v
  aStruct   :: Ty -> [(FldName,v)] -> v

  -- | Reading (purely) from array values
  -- | NB: Ty is the *result* type of the projection
  aArrRead  :: Ty -> v -> v -> LengthInfo -> v
  -- | Reading (purely) from struct values
  aStrProj  :: Ty -> v -> FldName -> v 

-- | Abstract values are either lvalues or concrete values
data AVal v = LVal (LVal v) | RVal v

rValM :: Monad m => v -> m (AVal v)
rValM x = return (RVal x)
lValM :: Monad m => LVal v -> m (AVal v)
lValM x = return (LVal x)

-- | Commands 
class CmdDom m v | m -> v where
  aAssign      :: LVal v -> v -> m ()
  aDerefRead   :: LVal v -> m v

  withImmABind :: EId -> v -> m v -> m v
  withMutABind :: EId -> m v -> m v

  aCall     :: EId -> [(AVal v)] -> m v
  aError    :: m a
  aPrint    :: Bool -> [v] -> m ()

-- | Commands plus controls flow 
class CmdDom m v => CmdDomRec m v | m -> v where 
  aWhile  :: (POrd s, Monad m, MonadState s m, ValDom v)
          => Exp -> m v -> m v
  aFor    :: (POrd s, Monad m, MonadState s m, ValDom v)
          => EId -> Exp -> Exp -> m v -> m v
  aBranch :: (POrd s, Monad m, MonadState s m, ValDom v) 
          => Exp -> m v -> m v -> m v

-- | Specific operations for abstract domains
class AbsInt m v | m -> v where

  aTrace :: m () -- For debugging

  aSkip  :: m v
  aJoin  :: m v -> m v -> m v
  aWithFact :: v -> m v -> m v

  aWiden :: v -> m v -> m v -> m v
  -- | Default implementation
  aWiden v m1 m2 = aJoin (aWithFact v m1) m2

-- | Run computation w/o modifying initial state and return final state
inCurrSt :: MonadState s m => m a -> m (a,s)
inCurrSt act = do
  pre  <- get
  x    <- act
  post <- get
  put pre
  return (x,post)

{------------------------------------------------------------------------
  For abstract (as opposed to concrete) interpreter DomRec is definable
------------------------------------------------------------------------}

-- | A newtype for /abstract/ interpreters
newtype AbsT m a = AbsT { unAbsT :: m a } 
  deriving (Applicative, Functor)

instance AbsInt m a => AbsInt (AbsT m) a where
  aSkip                        = AbsT aSkip
  aJoin (AbsT m1) (AbsT m2)    = AbsT (aJoin m1 m2)
  aWithFact v (AbsT m)         = AbsT (aWithFact v m)
  aWiden v (AbsT m1) (AbsT m2) = AbsT (aWiden v m1 m2)
  aTrace                       = AbsT (aTrace :: m ())

instance CmdDom m v => CmdDom (AbsT m) v where
  aAssign lval val          = AbsT (aAssign lval val)
  aDerefRead lval           = AbsT (aDerefRead lval)
  withImmABind x v (AbsT m) = AbsT (withImmABind x v m)
  withMutABind x (AbsT m)   = AbsT (withMutABind x m)
  aCall f vargs             = AbsT (aCall f vargs)
  aError                    = AbsT (aError)
  aPrint b vs               = AbsT (aPrint b vs)


instance (AbsInt m v, CmdDom m v) => CmdDomRec (AbsT m) v where
  aBranch e m1 m2 = do 
    a <- absEvalRVal e
    aJoin (aWithFact a m1) (aWithFact (aUnOp Not a) m2)
  
  aWhile e m = afix (aTrace :: AbsT m ()) $ do 
    a <- absEvalRVal e
    aWiden a (aWithFact a m) (aWithFact (aUnOp Not a) aSkip)

  -- NB: We could improve by statically inlining when elen is known
  aFor idx estart elen m =
   withMutABind idx $ do 
    astart <- absEvalRVal estart
    aAssign (varLVal idx) astart
    let econd = eBinOp noLoc Lt eidx (eBinOp noLoc Add estart elen)
    let erhs  = eBinOp noLoc Add eidx (eVal noLoc (nameTyp idx) (VInt 1))
        m'    = do x <- m 
                   arhs <- absEvalRVal erhs
                   aAssign (varLVal idx) arhs
                   return x
    aWhile econd m'

   where 
      eidx = eVar noLoc idx

afix :: (POrd s, MonadState s m) => m () -> m a -> m a
afix trace action = loop
  where loop = do
          trace -- just for debugging 
          pre <- get
          x <- action
          post <- get
          if post `pleq` pre then return x else loop


{---------------------------------------------------------------------------
  The abstract evaluator proper
---------------------------------------------------------------------------}


type EvalCtx s m v
  = ( POrd s
    , Monad m
    , MonadState s m
    , ValDom v
    , CmdDomRec m v 
    )

absEvalRVal :: EvalCtx s m v => Exp -> m v 
-- | Evaluate and if we get back an l-value then just read it
absEvalRVal  e = absEval e >>= do_ret
  where do_ret (RVal v)    = return v
        do_ret (LVal lval) = aDerefRead lval

absEvalLVal :: EvalCtx s m v => Exp -> m (LVal v) 
-- | Precondition: this expression does evaluate to an LValue
absEvalLVal e = do { r <- absEval e; return (fromLVal r) }
  where fromLVal :: AVal v -> LVal v
        fromLVal (LVal lval) = lval
        fromLVal _           = error "fromLVal"


absEval :: forall s m v. EvalCtx s m v => Exp -> m (AVal v)
absEval e = go (unExp e) where

  go :: Exp0 -> m (AVal v)

  go (EVal _ v)   = rValM (aVal v)

  go (EValArr vs) = do 
    avs <- mapM absEvalRVal vs
    rValM (aArr avs)
 
  go (EUnOp u e1) = do 
    a <- absEvalRVal e1
    rValM (aUnOp u a)

  go (EBinOp b e1 e2) = do
    a1 <- absEvalRVal e1
    a2 <- absEvalRVal e2
    rValM (aBinOp b a1 a2)

  go (EAssign elhs erhs) = do
    arhs <- absEvalRVal erhs
    alhs <- absEvalLVal elhs
    aAssign alhs arhs
    rValM (aVal VUnit)

  go (EArrWrite earr ei rng erhs) = -- will go away!
    go $ EAssign (eArrRead noLoc earr ei rng) erhs

  go (EVar x) = lValM (varLVal x)

  go (EArrRead earr ei rng) = do
    vi <- absEvalRVal ei
    d <- absEval earr
    case d of
      LVal lval -> lValM (GDArr lval vi rng)
      RVal aval -> rValM (aArrRead (ctExp e) aval vi rng)

  go (EProj e0 fld) = do
    d <- absEval e0
    case d of
      LVal lval -> lValM (GDProj lval fld)
      RVal sval -> rValM (aStrProj (ctExp e) sval fld)

  go (EStruct nm tfs) = do
    atfs <- mapM eval_fld tfs
    rValM (aStruct nm atfs)
    where eval_fld (f,x) = absEvalRVal x >>= \v -> return (f, v)

  go (ELUT _ e1) = absEval e1

  go (EPrint nl es) = do 
    as <- mapM absEvalRVal es 
    aPrint nl as
    rValM (aVal VUnit)

  go (EError {}) = aError 

  go (ECall fn es) = do
    let (TArrow funtys _funres) = nameTyp fn
    let tys_args = zip funtys es
    as <- mapM absEvalArg tys_args
    -- Functions return by-value!
    aCall fn as >>= rValM 

  go (ESeq e1 e2) = do 
    _a1 <- absEval e1
    absEvalRVal e2 >>= rValM

  go (ELet v _ e1 e2) = do
    a1 <- absEvalRVal e1
    withImmABind v a1 (absEvalRVal e2) >>= rValM

  go (ELetRef v Nothing e2) = 
    withMutABind v (absEvalRVal e2) >>= rValM

  go (ELetRef v (Just e1) e2) = do
    a1 <- absEvalRVal e1
    withMutABind v ( do
       d <- absEvalLVal (eVar noLoc v)
       aAssign d a1
       absEvalRVal e2 ) >>= rValM

  -- | Control flow returns values!
  go (EIf ec e1 e2) 
    = aBranch ec (absEvalRVal e1) (absEvalRVal e2) >>= rValM

  go (EFor _ui ix estart elen ebody) 
    = aFor ix estart elen (absEvalRVal ebody) >>= rValM

  go (EWhile econd ebody)            
    = aWhile econd (absEvalRVal ebody) >>= rValM


absEvalArg :: forall m s v.  EvalCtx s m v => (ArgTy, Exp) -> m (AVal v)
absEvalArg (GArgTy _ Mut, earg) = do 
  v <- absEvalLVal earg
  return (LVal v) -- NB: /force/ it to be lvalue
absEvalArg (_, earg) = do
  v <- absEvalRVal earg
  return (RVal v) -- NB: /force/ a read if lvalue
