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
 , inCurrSt
) where


import AstExpr
import AstUnlabelled
import Utils
import PpExpr ()
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad ( unless )
import Data.Loc
import Data.Set ( Set )
import qualified Data.Set as Set
import NameEnv

import Text.PrettyPrint.HughesPJ

import CtExpr ( ctExp )
import Outputable 



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


{-------------------------------------------------------------------------------
  Type classes for the abstract domain
-------------------------------------------------------------------------------}

-- | Value domain
class ValDom v where 
  aVal      :: Val -> v
  aArr      :: [v] -> v
  aUnOp     :: UnOp -> v -> v
  aBinOp    :: BinOp -> v -> v -> v
  aStruct   :: Ty -> [(FldName,v)] -> v

-- | Commands 
class CmdDom m v | m -> v where
  aAssign      :: LVal v -> v -> m ()
  aDerefRead   :: LVal v -> m v

  withImmABind :: EId -> v -> m v -> m v
  withMutABind :: EId -> m v -> m v

  aCall     :: EId -> [Either v (LVal v)] -> m v
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
class AbsInt m v where

  aSkip     :: m v
  aJoin     :: m v -> m v -> m v 
  aWithFact :: v -> m v -> m v
  aWiden    :: v -> m v -> m v -> m v

-- | Run computation w/o modifying initial state
-- and return the final state
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
  aSkip = AbsT aSkip
  aJoin (AbsT m1) (AbsT m2) = AbsT (aJoin m1 m2)
  aWithFact v (AbsT m) = AbsT (aWithFact v m)
  aWiden v (AbsT m1) (AbsT m2) = AbsT (aWiden v m1 m2)

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
    a <- absEval e
    aJoin (aWithFact a m1) (aWithFact (aUnOp Not a) m2)
  
  aWhile e m = do
     a <- absEval e
     afix $ aWiden a (aWithFact a m)
                     (aWithFact (aUnOp Not a) aSkip)

  aFor idx estart elen m = do
    s <- get 
    astart <- absEval estart
    s' <- get
    unless (s' `pleq` s) $ fail "Analysis failure, imperative estart!"
    let econd = eBinOp noLoc Lt eidx (eBinOp noLoc Add estart elen)
    acond <- absEval econd
    let m' = m >> do rhs <- absEval $
                            eBinOp noLoc Add eidx
                              (eVal noLoc (nameTyp idx) (VInt 1))
                     aAssign (varLVal idx) rhs
                     return $ aVal VUnit
    withMutABind idx $
      do aAssign (varLVal idx) astart
         afix $ aWiden acond (aWithFact acond m')
                             (aWithFact (aUnOp Not acond) aSkip)
    where 
      eidx = eVar noLoc idx

afix :: (POrd s, MonadState s m) => m a -> m a
afix action = loop
  where loop = do
          pre <- get
          x <- action
          post <- get
          if post `pleq` pre then return x else loop

{-------------------------------------------------------------------------------
  The abstract evaluator proper
-------------------------------------------------------------------------------}

absEvalDeref :: ( POrd s, ValDom v, Monad m, MonadState s m, CmdDomRec m v )
             => Exp -> m (LVal v)
absEvalDeref e = go (unExp e) where
  -- Variables are lvalues
  go (EVar x) = return (GDVar x)
  -- Reading, projections
  go (EArrRead earr estart elen) = do
    d <- absEvalDeref earr
    astart <- absEval estart
    return (GDArr d astart elen)
  go (EProj e' fld) = do
    d <- absEvalDeref e'
    return (GDProj d fld)
  -- Allocations 
  go (EValArr vs) = do 
    let ty = ctExp e
    avs <- mapM absEval vs
    return $ GDNewArray ty avs
  go (EStruct t tfs) = do
    atfs <- mapM (\(f,x) -> absEval x >>= \a -> return (f,a)) tfs
    return $ GDNewStruct t atfs

  -- All rest should not occur here!
  go _ = panic $ 
         vcat [ text "BUG: Cannot evaluate expression as lvalue."
              , text "Should have been enforced by the frontend!"
              , nest 2 $ ppr e ]

absEval :: forall m s v. 
  ( POrd s, ValDom v, Monad m, MonadState s m, CmdDomRec m v )
        => Exp -> m v
absEval e = go (unExp e) where
  -- Values and operators
  go (EVal _ v)   = return $ aVal v
  go (EValArr vs) = do 
    avs <- mapM absEval vs
    return $ aArr avs
  go (EVar x)     = aDerefRead $ varLVal x
  go (EUnOp u e1) = do 
    a <- absEval e1
    return $ aUnOp u a
  go (EBinOp b e1 e2) = do
    a1 <- absEval e1
    a2 <- absEval e2
    return $ aBinOp b a1 a2
  -- Assignments 
  go (EAssign elhs erhs) = do
    arhs <- absEval erhs
    alhs <- absEvalDeref elhs
    aAssign alhs arhs
    return $ aVal VUnit
  go (EArrWrite earr ei rng erhs) = 
    go $ EAssign (eArrRead noLoc earr ei rng) erhs

  -- Reading, projections 
  go (EArrRead {}) = do
    d <- absEvalDeref e
    aDerefRead d
  go (EProj {}) = do
    d <- absEvalDeref e
    aDerefRead d

  go (EStruct nm tfs) = do
    atfs <- mapM (\(f,x) -> absEval x >>= \a -> return (f,a)) tfs
    return $ aStruct nm atfs

  go (ELUT _ e1)    = absEval e1
  go (EPrint nl es) = do 
    as <- mapM absEval es 
    aPrint nl as
    return $ aVal VUnit

  go (EError {})   = aError 

  go (ECall fn es) = do
    let (TArrow funtys _funres) = nameTyp fn
    let tys_args = zip funtys es
    as <- mapM absEvalArg tys_args
    aCall fn as

  go (ESeq e1 e2) = do 
    _a1 <- absEval e1
    absEval e2

  go (EIf ec e1 e2) = do
    aBranch ec (absEval e1) (absEval e2) 

  go (ELet v _ e1 e2) = do
    a1 <- absEval e1
    withImmABind v a1 $ absEval e2

  go (ELetRef v Nothing e2) = do
    withMutABind v $ absEval e2

  go (ELetRef v (Just e1) e2) = do
    a1 <- absEval e1
    withMutABind v $ do
      d <- absEvalDeref (eVar noLoc v)
      aAssign d a1
      absEval e2

  go (EFor _ui ix estart elen ebody) = do   
    aFor ix estart elen $ absEval ebody
    return $ aVal VUnit

  go (EWhile econd ebody) = do
    aWhile econd (absEval ebody)
    return $ aVal VUnit


absEvalArg :: forall m s v. 
  ( POrd s, ValDom v, Monad m, MonadState s m, CmdDomRec m v)
           => (ArgTy,Exp) -> m (Either v (LVal v))
absEvalArg (GArgTy _ Mut, earg) 
  = absEvalDeref earg >>= (return . Right)
absEvalArg (_, earg)            
  = absEval      earg >>= (return . Left )
