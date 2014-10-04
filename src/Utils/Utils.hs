{-# OPTIONS_GHC -Wall #-}
-- | Generic utils
module Utils (
    panic
  , mapKeysM
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

mapKeysM :: (Monad m, Ord k2) => (k1 -> m k2) -> Map k1 a -> m (Map k2 a)
mapKeysM f mp = do
  let kvs = Map.toList mp
  kvs' <- mapM (\(k, v) -> do k' <- f k ; return (k', v)) kvs
  return $ Map.fromList kvs'
