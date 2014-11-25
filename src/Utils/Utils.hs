{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
-- | Generic utils
module Utils (
    panic
  , panicStr
  , mapKeysM
  , mapTelescope
  , parsePragmaLine
  , uncurry4 
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



{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

uncurry4 :: (a -> b -> c -> d -> z) -> (a, b, c, d) -> z
uncurry4 fun (a, b, c, d) = fun a b c d
