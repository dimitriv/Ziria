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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Analysis.UseDef
  ( VarTy
  , inOutVars
  , varUseDefs
  ) where

import AstExpr
import Analysis.Range

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State  (MonadState(..), gets, modify)
import Data.List ((\\), foldl', nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

-- XXX
--
-- This needs to be rewritten as a proper backwards analysis.

type VarSet = Set Name

type VarTy = (Name, Ty)

data UDEnv = UDEnv { flowvars :: Set Name    -- ^ Variables that influenced
                                             -- control flow
                   }

udenv0 :: UDEnv
udenv0 = UDEnv Set.empty

data UDState = UDState { usedefs  :: Map Name VarSet -- ^ In-scope variables and
                                                     -- the /free/ variables
                                                     -- used to imperatively
                                                     -- define them.
                       , usetys   :: Map.Map Name Ty -- ^ Types of in-scope
                                                     -- variables.
                       }

udstate0 :: UDState
udstate0 = UDState Map.empty Map.empty
             
newtype UDM a = UDM { runUDM :: UDEnv -> UDState -> Either String (a, UDState) }

instance Monad UDM where
    return a = UDM $ \_ s -> Right (a, s)
    m1 >> m2 = UDM $ \env s -> case runUDM m1 env s of
                                 Left  err     -> Left err
                                 Right (_, s') -> runUDM m2 env s'
    m1 >>= k = UDM $ \env s -> case runUDM m1 env s of
                                 Left  err     -> Left err
                                 Right (a, s') -> runUDM (k a) env s'
    fail err = UDM $ \_ _ -> Left err

instance Functor UDM where
    fmap f x = x >>= return . f

instance Applicative UDM where
    pure   = return
    (<*>)  = ap

instance MonadReader UDEnv UDM where
    ask = UDM $ \env s -> Right (env, s)

    local f m = UDM $ \env s -> runUDM m (f env) s

    reader f = UDM $ \env s -> Right (f env, s)

instance MonadState UDState UDM where
    get   = UDM $ \_ s -> Right (s,  s)
    put s = UDM $ \_ _ -> Right ((), s)

extendFlowVars :: VarSet -> UDM a -> UDM a
extendFlowVars vs =
    local $ \env -> env { flowvars = flowvars env `Set.union` vs }

extendVars :: [(Name, Ty)] -> UDM a -> UDM a
extendVars vs m = do
    a <- m
    modify $ \s -> s { usedefs = foldl' (flip Map.delete) (usedefs s) (map fst vs)
                     , usetys  = foldl' (flip Map.delete) (usetys s)  (map fst vs)
                     }
    return a

insertUseDefs :: Name -> VarSet -> UDM ()
insertUseDefs v vs = do
    flowvs <- asks flowvars
    modify $ \s -> s { usedefs = Map.insert v (vs `Set.union` flowvs) (usedefs s) }

insertUseTy :: Name -> Ty -> UDM ()
insertUseTy v ty =
    modify $ \s -> s { usetys  = Map.insert v ty (usetys s) }

lookupVarDef :: Name -> UDM VarSet
lookupVarDef v = do
    maybe_vs <- gets $ \s -> Map.lookup v (usedefs s)
    case maybe_vs of
      Nothing -> return $ Set.singleton v -- In this case 'v' is a free variable
      Just vs -> return vs

varUseDefs :: Monad m
           => Map Name Range
           -> Exp Ty -- ^ Expression to analyze
           -> m ([VarTy], [VarTy], [VarTy], [VarTy])
              -- ^ Returns free variables that were modified, free variable used
              -- to make imperative modifications, free variables used to
              -- calculate the pure result of the expression, and all free
              -- variables used.
varUseDefs ranges e =
    case runUDM (go e) udenv0 udstate0 of
      Left err -> fail err
      Right (vs, udstate) ->
          let modified, impUsed, pureUsed :: [Name]
              modified = Map.keys (usedefs udstate)
              impUsed  = Set.toList $ foldl' Set.union Set.empty (Map.elems (usedefs udstate))
              pureUsed = Set.toList vs

              allVars :: Map.Map Name Ty
              allVars =  usetys udstate

              look :: Name -> VarTy
              look v = case Map.lookup v allVars of
                         Just ty -> (v, ty)
                         Nothing -> error $ "Cannot find " ++ show v ++ " in type environment"
          in
            return (map look modified, map look impUsed, map look pureUsed, Map.toList allVars)
  where
    -- Returns the set of variables used to calculate the pure part of the
    -- expression being analyzed.
    go :: Exp Ty -> UDM VarSet
    go (MkExp (EVal _) _ _)         = return mempty
    go (MkExp (EValArr _) _ _)      = return mempty
    go (MkExp (EVar v) _ ty)        = do insertUseTy v ty
                                         lookupVarDef v
    go (MkExp (EUnOp _ e) _ _)      = go e
    go (MkExp (EBinOp _ e1 e2) _ _) = (<>) <$> go e1 <*> go e2

    -- go (MkExp (EComplex e1 e2) _ _) = (<>) <$> go e1 <*> go e2

    go (MkExp (EAssign (MkExp (EVar v) _ ty) e2) _ _) = do
        vs <- go e2
        insertUseTy v ty
        insertUseDefs v vs
        return mempty

    go (MkExp (EAssign {}) _ _) =
        fail "Illegal assignment expression: assignment to an expression"

    go (MkExp (EArrRead earr ei _) _ _) = do
        earr_use <- go earr
        ei_use   <- go ei
        return $ earr_use <> ei_use

    -- Bit permutation is like an array read
    go (MkExp (EBPerm earr ei) _ _) = do
        earr_use <- go earr
        ei_use   <- go ei
        return $ earr_use <> ei_use

    go (MkExp (EArrWrite (MkExp (EVar v) _ ty) ei len e) _ _) = do
        insertUseTy v ty
        vs0 <- lookupVarDef v
        vs  <- (<>) <$> go ei <*> go e
        updateArrUseDef v vs0 vs
        return mempty
      where
        updateArrUseDef :: Name -> VarSet -> VarSet -> UDM ()
        updateArrUseDef v vs0 vs | TArr (Literal n) _ <- ty
                                 , Just (Range l h)   <- arrIdxRange ranges ei len
                                 , l == 0 && h == fromIntegral n - 1 = do
            insertUseDefs v vs

        updateArrUseDef v vs0 vs =
            insertUseDefs v (vs0 `Set.union` vs)

    go (MkExp (EArrWrite {}) _ _) =
        fail "Illegal array write expression: array is an expression"

    go (MkExp (EIter ix x earr ebody) _ _) = do
        tarr <- case info earr of
                  TArr _ tarr -> return tarr
                  _           -> fail "Array type expected"
        extendVars [(ix, tint32)
                   ,(x,  tarr)] $ do
        vs_earr <- go earr
        insertUseDefs ix Set.empty
        insertUseDefs x  vs_earr
        (Set.delete ix . Set.delete x) <$> go ebody

    go (MkExp (EFor _ ix estart elen ebody) _ _) =
        extendVars [(ix, tint32)] $ do
        vs_ix <- (<>) <$> go estart <*> go elen
        insertUseDefs ix vs_ix
        Set.delete ix <$> go ebody

    go (MkExp (EWhile econd ebody) _ _) = do
        u1 <- go econd
        u2 <- go ebody
        return $ u1 <> u2

    go (MkExp (ELet v e1 e2) _ _) = do
        vs_e1 <- go e1
        extendVars [(v, info e1)] $ do
        insertUseDefs v vs_e1
        Set.delete v <$> go e2

    go (MkExp (ELetRef v e1 e2) _ _) = do
        (ty, vs_e1) <- 
           case e1 of Left ty   -> return (ty, mempty)
                      Right e1' -> go e1' >>= \r -> return (info e1', r)
        extendVars [(v,ty)] $ do
        insertUseDefs v vs_e1
        Set.delete v <$> go e2
        
    -- For ESeq we throw away the variables used to calculate the pure part of
    -- 'e1'.
    go (MkExp (ESeq e1 e2) _ _) = do
        _ <- go e1
        go e2

    go (MkExp (ECall {}) _ _) =
        fail "Cannot handle call"

    go (MkExp (EIf econd ethen eelse) _ _) = do
      vs_cond <- go econd
      ud0     <- gets usedefs

      -- Run the then branch
      vs_then <- extendFlowVars vs_cond $ go ethen
      ud_then <- gets usedefs

      -- Put back the use-def state that was in effect before the then branch
      modify $ \s -> s { usedefs = ud0 }

      -- Run the else branch
      vs_else <- extendFlowVars vs_cond $ go eelse
      ud_else <- gets usedefs

      -- Join the effects from the two branches
      modify $ \s -> s { usedefs = Map.unionWith Set.union ud_then ud_else }

      return $ mconcat [vs_cond, vs_then, vs_else]

    go (MkExp (EPrint {}) _ _) =
        return mempty

    go (MkExp (EError {}) _ _) =
        return mempty

    go (MkExp (ELUT {}) _ _) =
        fail "Cannot handle a recursive LUT"

    go (MkExp (EStruct fn tfs) _ _) = do
        us <- mapM (\(fn,e) -> go e) tfs
        let comb [u]     = u
            comb (u1:us) = u1 <> comb us
            comb []      = error "Can't happen!"
        return $ comb us
    go (MkExp (EProj e fn)_ _) = go e 


inOutVars :: Monad m
          => [(Name,Ty)]
          -> Map Name Range
          -> Exp Ty
          -> m ([VarTy], [VarTy], [VarTy])
inOutVars locals ranges e = do
    (modified, impUsed, pureUsed, allVars) <- varUseDefs ranges e
    let inVars, outVars :: [VarTy]
        inVars  = nub $ impUsed ++ pureUsed
        outVars = modified \\ locals
    return (inVars, outVars, allVars)
