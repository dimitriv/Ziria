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
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
-- | Range analysis
module Analysis.Range
  ( Range(..)
  , pprRanges
  , varRanges
  , expRange
  , arrIdxRange
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Data (Data)
import Data.Map (Map)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland
import Text.Show.Pretty (PrettyVal)
import qualified Data.Map as Map

import AstExpr
import CtExpr
import Utils

{-------------------------------------------------------------------------------
  Definition of the Range
-------------------------------------------------------------------------------}

-- @Range i j@ means that we know /all/ values in the range will be taken on.
data Range = RangeTop
           | Range Integer Integer
  deriving (Generic, Typeable, Data, Eq, Show)

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

instance PrettyVal Range

pprRanges :: Map (GName Ty) Range -> Doc
pprRanges r = stack $
    map (\(k,v) -> ppr k <> char ':' <+> ppr v) (Map.toList r)

joinR :: Range -> Range -> Range
joinR RangeTop      _             = RangeTop
joinR _             RangeTop      = RangeTop
joinR (Range l1 h1) (Range l2 h2) = Range (min l1 l2) (max h1 h2)

{-------------------------------------------------------------------------------
  Range monad and infrastructure
-------------------------------------------------------------------------------}

data RState = RState { ranges :: Map (GName Ty) Range }

newtype R a = R (ErrorT String (State RState) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState RState
           , MonadError String
           )

runR :: Monad m => RState -> R a -> m (a, RState)
runR st (R r) = case runState (runErrorT r) st of
                  (Left err,  _) -> fail err
                  (Right a, st') -> return (a, st')

setRange :: GName Ty -> Range -> R ()
setRange v r = modify $ \s -> s { ranges = Map.insert v r (ranges s) }

joinRange :: GName Ty -> Range -> R ()
joinRange v r1 = modify $ \s -> s { ranges = Map.alter f v (ranges s) }
  where
    f Nothing   = Just r1
    f (Just r2) = Just $ r1 `joinR` r2

lookupRange :: GName Ty -> R Range
lookupRange v = gets $ \s -> case Map.lookup v (ranges s) of
                   Nothing -> RangeTop
                   Just r  -> r

{-------------------------------------------------------------------------------
  Various ways of calling the analysis
-------------------------------------------------------------------------------}

varRanges :: Monad m => Exp -> m (Map (GName Ty) Range)
varRanges = liftM (ranges . snd) . runR (RState Map.empty) . erange

expRange :: Monad m => Map (GName Ty) Range -> Exp -> m Range
expRange rs = liftM fst . runR (RState rs) . erange

arrIdxRange :: Monad m
            => Map (GName Ty) Range
            -> Exp
            -> LengthInfo
            -> m Range
arrIdxRange rs = \e len -> liftM fst $ runR (RState rs) (go e len)
  where
    go :: Exp -> LengthInfo -> R Range
    go e LISingleton = erange e

    go e (LILength len) | EBinOp Mult e1 e2 <- unExp e
                        , EVal _ (VInt k) <- unExp e2
                        , fromIntegral k == len = do
        let k' = fromIntegral k
        r <- erange e1
        case r of
          RangeTop  -> return RangeTop
          Range l h -> return $ Range (l*k') (h*k'+k'-1)

    go _ (LILength _) = return RangeTop
    go _ (LIMeta   _) = panicStr "arrIdxRange: meta-variable"

{-------------------------------------------------------------------------------
  The range analysis proper

  TODO: This definition is somewhat odd, in the sense that virtually all
  recursive calls to `erange` throw their result away. Maybe we should write
  this slightly differently. For now, have enabled -fno-warn-unused-do-bind.
-------------------------------------------------------------------------------}

erange :: Exp -> R Range
erange (MkExp (EVal _ (VInt i)) _ _) =
    return $ Range (fromIntegral i) (fromIntegral i)

erange (MkExp (EVal _ _) _ _) =
    return RangeTop

erange (MkExp (EValArr _ _) _ _) =
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

erange (MkExp (EArrWrite (MkExp (EVar _) _ _) ei _ e) _ _) = do
    erange ei
    erange e
    return RangeTop

erange (MkExp (EArrWrite {}) _ _) =
    fail "Illegal array write expression: array is an expression"

erange (MkExp (EIter ix _ earr ebody) _ _) | TArray (Literal n) _ <- ctExp earr = do
    erange earr
    setRange ix (Range 0 (fromIntegral n - 1))
    erange ebody
    return RangeTop

erange (MkExp (EIter _ _ earr ebody) _ _) = do
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
           Nothing  -> return RangeTop
           Just e1' -> erange e1'
    setRange v r
    erange e2

-- For ESeq we throw away the variables used to calculate the pure part of
-- 'e1'.
erange (MkExp (ESeq e1 e2) _ _) = do
    erange e1
    erange e2

erange (MkExp (ECall _ es) _ _) = do
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

erange (MkExp (EProj e _) _ _) = do
    erange e
    return RangeTop

erange (MkExp (EStruct _ tfs) _ _) = do
    mapM_ (erange . snd) tfs
    return RangeTop
