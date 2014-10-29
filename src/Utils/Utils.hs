{-# OPTIONS_GHC -Wall #-}
-- | Generic utils
module Utils (
    panic
  , mapKeysM
  , parsePragmaLine
  ) where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Maybe (listToMaybe)
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

parsePragmaLine :: String -> Maybe (Int, String, Maybe Int)
parsePragmaLine = maybeReadM $
    (,,) <$ expectString "#" <*> readsM <*> readsM <*> optional readsM

{-------------------------------------------------------------------------------
  Monadic interface to ReadS
-------------------------------------------------------------------------------}

maybeReadM :: ReadsM a -> String -> Maybe a
maybeReadM parser = listToMaybe . map fst . runReadsM parser

readsM :: Read a => ReadsM a
readsM = ReadsM reads

expectString :: String -> ReadsM ()
expectString = \str -> ReadsM (str `isPrefixOf`)
  where
    isPrefixOf :: String -> String -> [((), String)]
    []     `isPrefixOf` ys     = return ((), ys)
    _      `isPrefixOf` []     = []
    (x:xs) `isPrefixOf` (y:ys) = guard (x == y) >> xs `isPrefixOf` ys

-- | Monadic wrapper around ReadS
newtype ReadsM a = ReadsM { runReadsM :: ReadS a }

instance Functor ReadsM where
  fmap = liftM

instance Applicative ReadsM where
  pure  = return
  (<*>) = ap

instance Alternative ReadsM where
  empty = mzero
  (<|>) = mplus

instance Monad ReadsM where
  return a = ReadsM $ \str -> [(a, str)]
  x >>= f  = ReadsM $ \str -> concatMap (\(a, str') -> runReadsM (f a) str')
                                        (runReadsM x str)

instance MonadPlus ReadsM where
  mzero     = ReadsM $ \_   -> []
  mplus a b = ReadsM $ \str -> runReadsM a str ++ runReadsM b str
