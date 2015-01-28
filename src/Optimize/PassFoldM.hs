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
{-# OPTIONS_GHC -Wall -Wwarn #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, 
    GeneralizedNewtypeDeriving, MultiWayIf, QuasiQuotes, DeriveGeneric #-}
module PassFoldM where

import Prelude hiding (exp)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (isJust)
import GHC.Generics
import System.CPUTime
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJ
import Text.Printf
import Text.Show.Pretty (PrettyVal)
import qualified Data.Map as Map

import AstComp
import AstExpr
import AstUnlabelled
import Opts
import Outputable
import PpComp ()
import PpExpr ()
import qualified GenSym as GS

{-------------------------------------------------------------------------------
  Rewriting monad
-------------------------------------------------------------------------------}

data IsRewritten = NotRewritten | Rewritten

-- The rewriting monad keeps track of whether any rewriting actually happened
newtype RwM a 
  = RwM { unRwM :: StateT IsRewritten (ReaderT (GS.Sym, DynFlags) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runRwM :: RwM a -> (GS.Sym, DynFlags) -> IO (a, IsRewritten)
runRwM act = runReaderT (runStateT (unRwM act) NotRewritten)

rewrite :: a -> RwM a
rewrite a = RwM $ do
    put Rewritten
    return a

genSym :: String -> RwM String
genSym prefix = RwM $ do
    gs   <- fst `liftM` ask
    str' <- liftIO $ GS.genSymStr gs
    return (prefix ++ str')

getDynFlags :: RwM DynFlags
getDynFlags = RwM $ snd `liftM` ask

debugFold :: RwM Bool
debugFold = (`isDynFlagSet` DebugFold) <$> getDynFlags

logStep :: String -> Maybe SourcePos -> String -> RwM ()
logStep pass pos str = do
    shouldLog <- debugFold
    when shouldLog $ RwM $ do
      liftIO $ putStrLn $ "* " ++ pass ++ ": " ++ ppr' pos ++ "\n" ++ str
  where
    ppr' (Just p) = show p ++ ": "
    ppr' Nothing  = ""

-- | Record if an action does a local rewrite
--
-- This does not change the overall behaviour of the action.
recordLocalRewrite :: RwM a -> RwM (a, Bool)
recordLocalRewrite (RwM act) = RwM $ do
    before <- get
    put $ NotRewritten -- Pretend no rewriting has yet occurred
    result <- act
    after  <- get
    case before of
      Rewritten    -> put Rewritten -- Restore state even if no local rewrites
      NotRewritten -> return ()     -- Leave state as set by the local action
    case after of
      Rewritten    -> return (result, True)
      NotRewritten -> return (result, False)

-- | New typed name generation in the optimizer
newPassFoldGName :: String -> ty -> Maybe SourcePos -> MutKind -> RwM (GName ty)
newPassFoldGName nm ty loc mk = do
    str <- genSym ""
    return $ (toName (nm++"_"++str) loc ty mk) {uniqId = MkUniq ("_pf"++str)}

{-------------------------------------------------------------------------------
  Rewriting statistics
-------------------------------------------------------------------------------}

newtype RwStats = RwStats { getRwStats :: Map.Map String RwStepStats }

data RwStepStats = RwStepStats {
    rw_invoked  :: Int
  , rw_rewrote  :: Int
  , rw_inv_time :: Double
  , rw_rew_time :: Double
  }

printRwStats :: RwStats -> IO ()
printRwStats mp =
    mapM_ print_one (Map.toList $ getRwStats mp)
  where
    print_one (pn, RwStepStats{..}) =
      printf "%20s:%d/%d/%f, %f\n" pn rw_invoked
                                      rw_rewrote
                                      rw_inv_time
                                      rw_rew_time

incInvokes :: RwStats -> Double -> String -> RwStats
incInvokes mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where
    aux Nothing                       = Just (RwStepStats 1 0 d 0)
    aux (Just (RwStepStats i r d0 t)) = Just (RwStepStats (i+1) r (d0+d) t)

incRewrites :: RwStats -> Double -> String -> RwStats
incRewrites mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where
    aux Nothing                       = Just (RwStepStats 1 1 0 d)
    aux (Just (RwStepStats i r _t f)) = Just (RwStepStats i (r+1) d (f+d))

{-------------------------------------------------------------------------------
  Top-level: definition of a pass, and infrastructure to run passes
-------------------------------------------------------------------------------}

-- | Transformations on Comp terms
data TypedCompPass =
    -- | Apply the pass on each node of the tree in bottom-up fashion
    TypedCompBottomUp (Maybe SourcePos -> Comp -> RwM Comp)

-- | Transformations on Exp terms
data TypedExpPass =
    -- | Apply the pass on each node of the tree in bototm-up fashion
    TypedExpBottomUp (Maybe SourcePos -> Exp -> RwM Exp)

    -- | The pass does its own traversal of the tree.
  | TypedExpManual (Exp -> RwM Exp)


newtype LetEs = LetEs [(Maybe SourcePos, GName Ty, ForceInline, Exp)]
  deriving (Generic)

-- | Collect multiple top-level consecutive `LetE` bindings
--
-- Returns `Nothing` if no top-level `LetE`s were found
extractCLetEs :: Comp -> Maybe (LetEs, Comp)
extractCLetEs = \comp -> do
    let (ls, suffix) = go comp
    guard $ not (null ls)
    return (LetEs ls, suffix)
  where
    go comp = case unComp comp of
      LetE nm fi e c' -> let (ls, suffix) = go c'
                         in ((compLoc comp, nm, fi, e) : ls, suffix)
      _               -> ([], comp)

-- | Add a series of `LetE` bindings to an expression
insertELetEs :: LetEs -> Exp -> Exp
insertELetEs = \(LetEs ls) -> go ls
  where
    go []                    e' = e'
    go ((loc, nm, fi, e):ls) e' = eLet loc nm fi e (go ls e')

newtype LetERefs = LetERefs [MutVar]
  deriving (Generic)

-- | Collect multiple top-level consecutive `LetERef` bindings
--
-- Returns `Nothing` if no top-level `LetERef`s were found
extractCMutVars' :: Comp -> Maybe (LetERefs, Comp)
extractCMutVars' c =
    case extractCMutVars c of
      ([], _)  -> Nothing
      (vs, c') -> Just (LetERefs vs, c')

-- | Add a series of `LetERef` bindings to an expression
insertEMutVars' :: LetERefs -> Exp -> Exp
insertEMutVars' (LetERefs vs) = insertEMutVars vs


{-------------------------------------------------------------------------------
  Some simple analyses
-------------------------------------------------------------------------------}

-- | Just a heuristic for inlining: what are 'simple' expressions that
-- are safe and likely beneficial to just inline before code generation.
is_simpl_expr :: Exp -> Bool
is_simpl_expr = go . unExp
  where
    go :: Exp0 -> Bool
    go (EVal _ _)       = True
    go (EValArr elems)  = all is_simpl_expr elems
    -- Immutable variables are OK to inline in any context
    go (EVar nm)        = nameMut nm == Imm 
    go (EUnOp _ e)      = is_simpl_expr e
    -- NB: No case for BinOp because this might duplicate computation
    go (EStruct _ fses) = all is_simpl_expr (map snd fses)
    -- We could even do the following:
    go (EArrRead earr estart _elen)
      = is_simpl_expr earr && is_simpl_expr estart
    go (EProj estruct _fld)
      = is_simpl_expr estruct
    go _                = False


no_lut_inside :: Exp -> Bool
no_lut_inside x = isJust (mapExpM return return elut_nothing x)
  where
    elut_nothing :: Exp -> Maybe Exp
    elut_nothing (MkExp (ELUT {}) _ ()) = Nothing
    elut_nothing other                  = Just other


{-------------------------------------------------------------------------------
  Outputable instances for the view patterns, above

  Useful for debugging
-------------------------------------------------------------------------------}

instance PrettyVal LetEs
instance PrettyVal LetERefs

instance Outputable LetEs where
  ppr (LetEs ls) = hsep (punctuate comma (map aux ls))
    where
      aux (_, nm, _, _e) = ppr nm <+> text "=" <+> text ".." -- ppr e

instance Outputable LetERefs where
  ppr (LetERefs ls) = hsep (punctuate comma (map aux ls))
    where
      aux MutVar{..} =
        case mutInit of
          Nothing -> ppr mutVar
          Just _e -> ppr mutVar <+> text "=" <+> text ".." -- ppr e

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Measure how long an action takes
measure :: MonadIO m => m a -> m (a, Double)
measure act = do
    st <- liftIO $ getCPUTime
    a  <- act
    en <- liftIO $ getCPUTime
    return (a, fromIntegral (en - st) / (10 ^ (12 :: Integer)))
