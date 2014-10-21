{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
-- | Generic utils
module Utils (
    panic
  , panicStr
  , mapKeysM
  , mapTelescope
  ) where

import Data.Map (Map)
import Text.PrettyPrint.HughesPJ
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import qualified Data.Map as Map

panic :: Doc -> a
panic err = unsafePerformIO $ do
  print $ text "Panic! The impossible happened!" $$ err
  exitFailure

panicStr :: String -> a
panicStr = panic . text

mapKeysM :: (Monad m, Ord k2) => (k1 -> m k2) -> Map k1 a -> m (Map k2 a)
mapKeysM f mp = do
  let kvs = Map.toList mp
  kvs' <- mapM (\(k, v) -> do k' <- f k ; return (k', v)) kvs
  return $ Map.fromList kvs'

-- | Monadic map over a telescope
--
-- A /telescope/ is a list of binding sites where later items in the list
-- can refer to earlier items the list. A typical example is a list of
-- parameters, where we might have
--
-- > fun comp f(arr int x, arr[length(x)] int y) { .. }
--
-- Typically when we map over a telescope we want to map each subsequent
-- in some sort of environment that records all previous elements. We
-- abstract over this pattenr here.
mapTelescope :: Monad m
             => (forall x. b -> m x -> m x)  -- ^ Environment extension
             -> (a -> m b)                   -- ^ Type check function
             -> [a]
             -> m [b]
mapTelescope ext tc = go
  where
    go []     = return []
    go (a:as) = do
      b  <- tc a
      bs <- ext b $ go as
      return (b:bs)
