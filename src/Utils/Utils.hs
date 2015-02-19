{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
-- | Generic utils
module Utils (
    panic
  , panicStr
  , warn
  , warnStr
  , assert
  , mapKeysM
  , mapTelescope
  , parsePragmaLine
  , uncurry4
  , groupBy'
  , (<|)
  , cross_prod
  , cross_comb
  , gcd_many
  , none
  ) where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Maybe ( listToMaybe )
import Text.PrettyPrint.HughesPJ
import System.IO.Unsafe ( unsafePerformIO )
import System.Exit
import qualified Data.Map as Map

panic :: Doc -> a
panic err = unsafePerformIO $ do
  print $ text "Panic! The impossible happened!" $$ err
  exitFailure

warn :: Doc -> a -> a 
warn msg a = unsafePerformIO $ do
  print $ text "Warning:" $$ msg
  return a

warnStr :: String -> a -> a
warnStr s a = warn (text s) a

panicStr :: String -> a
panicStr = panic . text


assert :: String -> Bool -> a -> a
assert _msg True x  = x
assert msg False _x = panicStr msg


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


-- | Data.List.groupBy does not merge non-consecutive equivalence
-- groups so we provide our own function that does exactly this.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = go xs []
 where
   go [] groups  = groups
   go (y:ys) groups = go ys (ginsert y groups)

   ginsert x [] = [[x]] -- create new singleton group
   ginsert x (g1:gs)
     | f x (head g1)
     = (x:g1):gs -- insert into g1
     | otherwise -- or else insert somewhere in gs or in the end
     = g1:(ginsert x gs)


-- | Composing two functions
(<|) :: (a -> a -> b) -> (c -> a) -> c -> c -> b
(<|) f g x y = f (g x) (g y)



-- | Cross-product of two lists
-- E.g. if xs = [ [a1,a2], [b1,b2,b3] ]
-- then cross_prod xs = [ [a1,b1], [a1,b2], [a1,b3], [a2,b1], [a2,b2], [a2,b3] ]
cross_prod :: [[a]] -> [[a]]
cross_prod = go [] []
  where
    go acc k []               = reverse acc : k
    go _acc _k ([]:_)         = panicStr "cross_prod: empty candidate list!"
    go acc k ([c1]:crest)     = go (c1:acc) k crest
    go acc k ((c1:c1s):crest) = go (c1:acc) (go acc k (c1s:crest)) crest

cross_comb :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
cross_comb f xs ys
  = [ c | x <- xs
        , y <- ys
        , c <- f' x y ]
  where f' a b = maybe [] (:[]) (f a b)

-- | GCD of many integers
gcd_many :: [Int] -> Int
gcd_many [a]        = a
gcd_many (a1:a2:as) = gcd_many (gcd a1 a2 : as)
gcd_many []         = error "gcd_many: empty list!"

-- | Check that none of the elements in the list satisfy the predicate
none :: (a -> Bool) -> [a] -> Bool
none p xs = not (any p xs)
