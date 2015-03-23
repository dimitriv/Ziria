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

   ValDom
 , CmdDom
 , CmdDomRec
 , AbsInt
 , AbsT  (..)
 , LVal
 , absEval 

) where


import AstExpr
import AstUnlabelled
import Utils
import PpExpr ()
import Control.Monad.State.Class
import Control.Applicative 
import Control.Monad ( when ) 

-- | Dereference expressions with abstract values as indices
type LVal idx = AGDerefExp idx Ty ()

-- | Variable lvalues 
varLVal :: EId -> LVal v
varLVal = GDVar Nothing ()


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
  aAssign   :: LVal v -> v -> m ()
  aDerefRead   :: LVal v -> m v

  withImmABind :: EId -> v -> m a -> m a
  withMutABind :: EId -> m a -> m a

  aCall     :: EId -> [Either v (LVal v)] -> m v
  aError    :: m a
  aPrint    :: Bool -> [v] -> m ()

-- | Commands plus controls flow 
class CmdDom m v => CmdDomRec m v | m -> v where 
  aWhile  :: (Ord s, Monad m, MonadState s m, ValDom v) 
          => Exp -> m v -> m v
  aFor    :: (Ord s, Monad m, MonadState s m, ValDom v) 
          => EId -> Exp -> Exp -> m v -> m v
  aBranch :: (Ord s, Monad m, MonadState s m, ValDom v) 
          => Exp -> m v -> m v -> m v

-- | Specific operations for abstract domains
class AbsInt m v where
  aSkip     :: m v
  aJoin     :: m v -> m v -> m v 
  aWithFact :: v -> m v -> m v

{------------------------------------------------------------------------
  For abstract (as opposed to concrete) interpreter DomRec is definable
------------------------------------------------------------------------}

-- | A newtype for /abstract/ interpreters
newtype AbsT m a = AbsT (m a) 
  deriving (Applicative, Functor)

instance AbsInt m a => AbsInt (AbsT m) a where
  aSkip = AbsT aSkip
  aJoin (AbsT m1) (AbsT m2) = AbsT (aJoin m1 m2)
  aWithFact v (AbsT m) = AbsT (aWithFact v m)

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
     afix $ aJoin (aWithFact a m)
                  (aWithFact (aUnOp Not a) aSkip)

  aFor idx estart elen m = do
    s <- get 
    astart <- absEval estart
    s' <- get
    when (not (s' <= s)) $ fail "Analysis failure, imperative estart!"
    let econd = eBinOp Nothing Lt eidx (eBinOp Nothing Add estart elen)
    acond <- absEval econd
    let m' = m >> do rhs <- absEval $
                            eBinOp Nothing Add eidx
                              (eVal Nothing (nameTyp idx) (VInt 1))
                     aAssign (varLVal idx) rhs
                     return $ aVal VUnit
    withMutABind idx $
      do aAssign (varLVal idx) astart
         afix $ aJoin (aWithFact acond m')
                      (aWithFact (aUnOp Not acond) aSkip)
    where 
      eidx = eVar Nothing idx

afix :: (Ord s, MonadState s m) => m a -> m a
afix action = loop
  where loop = do
          pre <- get
          x <- action
          post <- get
          if post <= pre then return x else loop

{-------------------------------------------------------------------------------
  The abstract evaluator proper
-------------------------------------------------------------------------------}
absEval :: forall m s v. 
  ( Ord s, ValDom v, Monad m, MonadState s m, CmdDomRec m v )
        => Exp -> m v
absEval e = go (unExp e) where
  -- Values and operators
  go :: Exp0 -> m v
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
    alhs <- absDeref elhs
    aAssign alhs arhs
    return $ aVal VUnit
  go (EArrWrite earr ei rng erhs) = 
    go $ EAssign (eArrRead Nothing earr ei rng) erhs

  -- Reading, projections 
  go (EArrRead {}) = do
    d <- absDeref e
    aDerefRead d
  go (EProj {}) = do
    d <- absDeref e
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
      d <- absDeref (eVar Nothing v)
      aAssign d a1
      absEval e2

  go (EFor _ui ix estart elen ebody) = do   
    aFor ix estart elen $ absEval ebody
    return $ aVal VUnit

  go (EWhile econd ebody) = do
    aWhile econd (absEval ebody)
    return $ aVal VUnit

absDeref :: forall m s v. 
  ( Ord s, ValDom v, Monad m, MonadState s m, CmdDomRec m v )
         => Exp -> m (LVal v)
absDeref e = case unExp e of
  EVar nm -> return $ GDVar loc nfo nm
  EProj estruct fld -> do 
    astruct <- absDeref estruct
    return $ GDProj loc nfo astruct fld
  EArrRead earr estart elen -> do
    astart <- absEval estart
    aearr <- absDeref earr
    return $ GDArr loc nfo aearr astart elen
  _ -> panicStr "absDeref: not a dereference expression!"
  where loc = expLoc e
        nfo = info e


absEvalArg :: forall m s v. 
  ( Ord s, ValDom v, Monad m, MonadState s m, CmdDomRec m v)
           => (ArgTy,Exp) -> m (Either v (LVal v))
absEvalArg (GArgTy _ Mut, earg) = absDeref earg >>= (return . Right)
absEvalArg (_, earg)            = absEval earg  >>= (return . Left )
