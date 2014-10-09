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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
-- | Use-Def analysis
--
-- See `varUseDefs` for an explanation of what this module is intended to
-- compute.
--
-- NOTE: It should probably be renamed, as it does not appear to be a
-- traditional use-def analysis
-- (http://en.wikipedia.org/wiki/Use-define_chain).
--
-- TODO: This needs to be rewritten as a proper backwards analysis.
module Analysis.UseDef (inOutVars) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List ((\\), foldl', nub)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstExpr
import Analysis.Range

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type VarSet = Set (GName Ty)

data UDEnv = UDEnv {
    -- | Variables that influenced control flow
    flowvars :: Set (GName Ty)
  }

udenv0 :: UDEnv
udenv0 = UDEnv Set.empty

data UDState = UDState {
    -- | In-scope variables and the /free/ variables used to imperatively
    -- define them.
    usedefs :: Map (GName Ty) VarSet

    -- | All free variables (currently in scope) that are actually used
  , usefree :: VarSet
  }

udstate0 :: UDState
udstate0 = UDState Map.empty Set.empty

{-------------------------------------------------------------------------------
  The UDM monad and infrastructure
-------------------------------------------------------------------------------}

newtype UDM a = UDM (ReaderT UDEnv (ErrorT String (State UDState)) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader UDEnv
           , MonadState UDState
           , MonadError String
           )

runUDM :: Monad m => UDEnv -> UDState -> UDM a -> m (a, UDState)
runUDM env st (UDM udm) =
  case runState (runErrorT (runReaderT udm env)) st of
    (Left err,  _) -> fail err
    (Right a, st') -> return (a, st')

-- TODO: This is currently _only_ called for conditions, and not, for example,
-- for loops. Is that intended?
extendFlowVars :: VarSet -> UDM a -> UDM a
extendFlowVars vs =
    local $ \env -> env { flowvars = flowvars env `Set.union` vs }

-- | Run an action, and then remove the given variables from the scope
extendVars :: [GName Ty] -> UDM a -> UDM a
extendVars vs m = do
    a <- m
    modify $ \s -> s { usedefs = foldl' (flip Map.delete) (usedefs s) vs
                     , usefree = foldl' (flip Set.delete) (usefree s) vs
                     }
    return a

insertUseDefs :: GName Ty -> VarSet -> UDM ()
insertUseDefs v vs = do
    flowvs <- asks flowvars
    modify $ \s -> s { usedefs = Map.insert v (vs `Set.union` flowvs) (usedefs s) }

insertUseFree :: GName Ty -> UDM ()
insertUseFree v =
    modify $ \s -> s { usefree = Set.insert v (usefree s) }

lookupVarDef :: GName Ty -> UDM VarSet
lookupVarDef v = do
    maybe_vs <- gets $ \s -> Map.lookup v (usedefs s)
    case maybe_vs of
      Nothing -> return $ Set.singleton v -- In this case 'v' is a free variable
      Just vs -> return vs

{-------------------------------------------------------------------------------
  The analysis proper
-------------------------------------------------------------------------------}

-- | Use-def analysis
--
-- Returns free variables that were modified, free variable used to make
-- imperative modifications, free variables used to calculate the pure result
-- of the expression, and all free variables used.
varUseDefs :: Monad m
           => Map (GName Ty) Range
           -> Exp -- ^ Expression to analyze
           -> m ([GName Ty], [GName Ty], [GName Ty], [GName Ty])
varUseDefs ranges = \e -> do
     (vs, udstate) <- runUDM udenv0 udstate0 (go e)

     let modified, impUsed, pureUsed :: [GName Ty]
         modified = Map.keys (usedefs udstate)
         impUsed  = Set.toList $ foldl' Set.union Set.empty (Map.elems (usedefs udstate))
         pureUsed = Set.toList vs

         allVars :: VarSet
         allVars = usefree udstate

     return (modified, impUsed, pureUsed, Set.toList allVars)
  where
    -- Returns the set of variables used to calculate the pure part of the
    -- expression being analyzed.
    go :: Exp -> UDM VarSet
    go = go0 . unExp

    go0 :: Exp0 -> UDM VarSet
    go0 (EVal _ _) =
        return mempty
    go0 (EValArr _ _) =
        return mempty
    go0 (EVar v) = do
        insertUseFree v
        lookupVarDef v
    go0 (EUnOp _ e) =
        go e
    go0 (EBinOp _ e1 e2) =
        (<>) <$> go e1 <*> go e2
    go0 (EAssign (MkExp (EVar v) _ _) e2) = do
        vs <- go e2
        insertUseFree v
        insertUseDefs v vs
        return mempty
    -- TODO: What about assignment to struct fields?
    go0 (EAssign {}) =
        fail "Illegal assignment expression: assignment to an expression"
    go0 (EArrRead earr ei _) = do
        earr_use <- go earr
        ei_use   <- go ei
        return $ earr_use <> ei_use
    -- Bit permutation is like an array read
    go0 (EBPerm earr ei) = do
        earr_use <- go earr
        ei_use   <- go ei
        return $ earr_use <> ei_use
    go0 (EArrWrite (MkExp (EVar v) _ _) ei len e) = do
        insertUseFree v
        vs0 <- lookupVarDef v
        vs  <- (<>) <$> go ei <*> go e
        updateArrUseDef vs0 vs
        return mempty
      where
        updateArrUseDef :: VarSet -> VarSet -> UDM ()
        updateArrUseDef _vs0 vs | TArray (Literal n) _ <- nameTyp v
                                , Just (Range l h)     <- arrIdxRange ranges ei len
                                , l == 0 && h == fromIntegral n - 1 = do
            insertUseDefs v vs
        updateArrUseDef vs0 vs =
            insertUseDefs v (vs0 `Set.union` vs)
    -- TODO: What about array-writes to structs?
    go0 (EArrWrite {}) =
        fail "Illegal array write expression: array is an expression"
    go0 (EIter ix x earr ebody) = do
        extendVars [ix, x] $ do
          vs_earr <- go earr
          insertUseDefs ix Set.empty
          insertUseDefs x  vs_earr
          (Set.delete ix . Set.delete x) <$> go ebody
    go0 (EFor _ ix estart elen ebody) =
        extendVars [ix] $ do
          vs_ix <- (<>) <$> go estart <*> go elen
          insertUseDefs ix vs_ix
          Set.delete ix <$> go ebody
    go0 (EWhile econd ebody) = do
        u1 <- go econd
        u2 <- go ebody
        return $ u1 <> u2
    go0 (ELet v _ e1 e2) = do
        vs_e1 <- go e1
        extendVars [v] $ do
          insertUseDefs v vs_e1
          Set.delete v <$> go e2
    go0 (ELetRef v me1 e2) = do
        vs_e1 <- case me1 of Nothing -> return mempty
                             Just e1 -> go e1
        extendVars [v] $ do
          insertUseDefs v vs_e1
          Set.delete v <$> go e2
    -- For ESeq we throw away the variables used to calculate the pure part of
    -- 'e1'.
    go0 (ESeq e1 e2) = do
        _ <- go e1
        go e2
    go0 (ECall {}) =
        fail "Cannot handle call"
    go0 (EIf econd ethen eelse) = do
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
    go0 (EPrint {}) =
        return mempty
    go0 (EError {}) =
        return mempty
    go0 (ELUT {}) =
        fail "Cannot handle a recursive LUT"
    go0 (EStruct _tn tfs) = do
        us <- mapM (\(_fn,e) -> go e) tfs
        return $ mconcat us
    go0 (EProj e _) =
        go e

inOutVars :: Monad m
          => [GName Ty]
          -> Map (GName Ty) Range
          -> Exp
          -> m ([GName Ty], [GName Ty], [GName Ty])
inOutVars locals ranges e = do
    (modified, impUsed, pureUsed, allVars) <- varUseDefs ranges e
    let inVars, outVars :: [GName Ty]
        inVars  = nub $ impUsed ++ pureUsed
        outVars = modified \\ locals
    return (inVars, outVars, allVars)
