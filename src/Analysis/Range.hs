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

module Analysis.Range
  ( Range(..)
  , pprRanges
  , varRanges
  , expRange
  , arrIdxRange
  ) where

import AstExpr

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.State  (MonadState(..), gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.Mainland

-- @Range i j@ means that we know /all/ values in the range will be taken on.
data Range = RangeTop
           | Range Integer Integer
  deriving (Eq, Show)

instance Num Range where
    RangeTop    + _           = RangeTop
    _           + RangeTop    = RangeTop
    Range l1 h1 + Range l2 h2 = Range (l1+l2) (h1+h2)

    negate RangeTop    = RangeTop
    negate (Range l h) = Range (-h) (-l)

    _ * _ = RangeTop

    abs _ = RangeTop

    signum _ = RangeTop

    fromInteger i = Range i i

instance Pretty Range where
    ppr = string . show

pprRanges :: Map Name Range -> Doc
pprRanges r = stack $
    map (\(k,v) -> ppr k <> char ':' <+> ppr v) (Map.toList r)

joinR :: Range -> Range -> Range
joinR RangeTop      _             = RangeTop
joinR _             RangeTop      = RangeTop
joinR (Range l1 h1) (Range l2 h2) = Range (min l1 l2) (max h1 h2)

data RState = RState { ranges :: Map Name Range }

newtype R a = R { runR :: RState -> Either String (a, RState) }

instance Monad R where
    return a = R $ \s -> Right (a, s)
    m1 >> m2 = R $ \s -> case runR m1 s of
                           Left  err     -> Left err
                           Right (_, s') -> runR m2 s'
    m1 >>= k = R $ \s -> case runR m1 s of
                           Left  err     -> Left err
                           Right (a, s') -> runR (k a) s'
    fail err = R $ \_ -> Left err

instance Functor R where
    fmap f x = x >>= return . f

instance Applicative R where
    pure   = return
    (<*>)  = ap

instance MonadState RState R where
    get   = R $ \s -> Right (s,  s)
    put s = R $ \_ -> Right ((), s)

setRange :: Name -> Range -> R ()
setRange v r =
    modify $ \s -> s { ranges = Map.insert v r (ranges s) }

joinRange :: Name -> Range -> R ()
joinRange v r1 =
    modify $ \s -> s { ranges = Map.alter f v (ranges s) }
  where
    f Nothing   = Just r1
    f (Just r2) = Just $ r1 `joinR` r2

lookupRange :: Name -> R Range
lookupRange v =
    gets $ \s -> case Map.lookup v (ranges s) of
                   Nothing -> RangeTop
                   Just r  -> r

varRanges :: Monad m
          => Exp Ty
          -> m (Map Name Range)
varRanges e =
    case runR (erange e) (RState Map.empty) of
      Left err -> fail err
      Right (_, RState ranges) ->
            return ranges

expRange :: Monad m
         => Map Name Range
         -> Exp Ty
         -> m Range
expRange ranges e =
    case runR (erange e) (RState ranges) of
      Left err -> fail err
      Right (r, _) ->
            return r

arrIdxRange :: Monad m
            => Map Name Range
            -> Exp Ty
            -> LengthInfo
            -> m Range
arrIdxRange ranges e len =
    case runR (go e len) (RState ranges) of
      Left err -> fail err
      Right (r, _) ->
            return r
  where
    go :: Exp Ty  -> LengthInfo -> R Range
    go e LISingleton =
        erange e

    go e (LILength l) | EBinOp Mult e1 e2 <- unExp e
                      , EVal (VInt k) <- unExp e2
                      , fromIntegral k == l = do
        let k' = fromIntegral k
        r <- erange e1
        case r of
          RangeTop  -> return RangeTop
          Range l h -> return $ Range (l*k') (h*k'+k'-1)

    go _ (LILength _) = return RangeTop

erange :: Exp Ty -> R Range
erange (MkExp (EVal (VInt i)) _ _) =
    return $ Range (fromIntegral i) (fromIntegral i)

erange (MkExp (EVal _) _ _) =
    return RangeTop

erange (MkExp (EValArr _) _ _) =
    return RangeTop

erange (MkExp (EVar v) _ _) =
    lookupRange v

erange (MkExp (EUnOp Neg e) _ _) =
    negate <$> erange e

erange (MkExp (EUnOp _ e) _ _) = do
    erange e
    return RangeTop

erange (MkExp (EBinOp Add e1 e2) _ _) = do
    r1 <- erange e1
    r2 <- erange e2
    return $ r1 + r2

erange (MkExp (EBinOp {}) _ _) =
    return RangeTop

-- erange (MkExp (EComplex e1 e2) _ _) = do
--     erange e1
--     erange e2
--     return RangeTop

erange (MkExp (EAssign (MkExp (EVar v) _ _) e2) _ _) = do
    r <- erange e2
    joinRange v r
    return r

erange (MkExp (EAssign {}) _ _) =
    fail "Illegal assignment expression: assignment to an expression"

erange (MkExp (EArrRead earr ei _) _ _) = do
    erange earr
    erange ei
    return RangeTop

erange (MkExp (EArrWrite (MkExp (EVar v) _ _) ei _ e) _ _) = do
    erange ei
    erange e
    return RangeTop

erange (MkExp (EArrWrite {}) _ _) =
    fail "Illegal array write expression: array is an expression"

erange (MkExp (EIter ix x earr ebody) _ _) | TArr (Literal n) _ <- info earr = do
    erange earr
    setRange ix (Range 0 (fromIntegral n - 1))
    erange ebody
    return RangeTop

erange (MkExp (EIter ix x earr ebody) _ _) = do
    erange earr
    erange ebody
    return RangeTop

erange (MkExp (EFor _ui ix estart elen ebody) _ _) = do
    r1 <- erange estart
    r2 <- erange elen
    setRange ix (r1 `joinR` (r2-1))
    erange ebody
    return RangeTop

erange (MkExp (EWhile econd ebody) _ _) = do
    erange econd
    erange ebody
    return RangeTop


erange (MkExp (ELet v _ e1 e2) _ _) = do
    r <- erange e1
    setRange v r
    erange e2

erange (MkExp (ELetRef v e1 e2) _ _) = do
    r <- case e1 of
           Left _    -> return RangeTop
           Right e1' -> erange e1'
    setRange v r
    erange e2

-- For ESeq we throw away the variables used to calculate the pure part of
-- 'e1'.
erange (MkExp (ESeq e1 e2) _ _) = do
    erange e1
    erange e2

erange (MkExp (ECall f es) _ _) = do
    erange f
    mapM_ erange es
    return RangeTop

erange (MkExp (EIf econd ethen eelse) _ _) = do
  erange econd
  r1 <- erange ethen
  r2 <- erange eelse
  return $ r1 `joinR` r2

erange (MkExp (EPrint {}) _ _) =
    return RangeTop

erange (MkExp (EError {}) _ _) =
    return RangeTop

erange (MkExp (ELUT _ e) _ _) =
    erange e

erange (MkExp (EBPerm e1 e2) _ _) = do
    erange e1
    erange e2
    return RangeTop

erange (MkExp (EProj e fn) _ _) = do
    erange e
    return RangeTop

erange (MkExp (EStruct _ tfs) _ _) = do
    mapM_ (erange . snd) tfs
    return RangeTop


