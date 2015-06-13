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
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses, 
             FunctionalDependencies, StandaloneDeriving, 
             GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

-- | Range analysis
module Analysis.RangeAnal 
     ( Range ( .. )
     , Interval ( .. )
     , RngMap
     , pprRanges
     , varRanges
     )
where

import Control.Applicative
import Control.DeepSeq.Generics (NFData(..), genericRnf)
import Control.Monad.Error
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJ

import AstExpr
import Orphans ()

import Outputable 
import PpExpr ()

import NameEnv
import AbsInt
import CtExpr

import Opts

import qualified Data.Set as S
import qualified Data.Monoid as M
import Text.Show.Pretty (PrettyVal)


{- The goal of the Range analysis
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Given an expression, calculate for every array variable:
     1) an overapproximation of the input range
     2) an exact output range or don't know (over or under approx not good)
   Later we will deal with structs too.
-}

{-------------------------------------------------------------------------------
  Intervals
-------------------------------------------------------------------------------}

data IVal v = IUnknown | IKnown v
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

instance Monad IVal where
  return a = IKnown a
  m >>= f  = case m of 
    IUnknown -> IUnknown
    IKnown x -> f x

iv_join_discrete :: Eq v => IVal v -> IVal v -> IVal v
iv_join_discrete IUnknown _ = IUnknown
iv_join_discrete _ IUnknown = IUnknown
iv_join_discrete (IKnown v1) (IKnown v2)
  | v1 == v2  = IKnown v1
  | otherwise = IUnknown

-- | Intervals
data Iv = Iv Integer Integer  -- i1 <= i2
        | IvEmpty
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

instance NFData Iv where
  rnf = genericRnf

type Interval = IVal Iv
instance NFData Interval where
  rnf = genericRnf

-- | Approximate interval join
ijoin_apx :: Interval -> Interval -> Interval 
ijoin_apx IUnknown _ = IUnknown
ijoin_apx _ IUnknown = IUnknown
ijoin_apx (IKnown (Iv i j)) (IKnown (Iv i' j')) 
  | let l = min i i'
  , let h = max j j'
  , let bound = 512
  = if l < - bound || h > bound then IUnknown else IKnown (Iv l h)


-- | Precise interval join
ijoin_pcs :: Interval -> Interval -> Interval
ijoin_pcs i1 i2 
  | i1 == i2  = i1
  | otherwise = IUnknown

istretch :: Bool -- ^ precise or not?
         -> Interval -> IVal Integer -> LengthInfo -> Interval 
istretch pcs _ IUnknown _ = IUnknown
istretch pcs IUnknown _ _ = IUnknown
istretch pcs (IKnown (Iv i j)) (IKnown s) LISingleton 
  = stretch pcs (i,j) (s,s)
istretch pcs (IKnown (Iv i j)) (IKnown s) (LILength n) 
  = stretch pcs (i,j) (s,s + fromIntegral n)

stretch :: Bool -> (Integer,Integer) -> (Integer,Integer) -> Interval 
stretch precise (a,b) (c,d)
  | precise && ( (c-b > 1) || (a-d > 1)) = IUnknown 
  | otherwise = IKnown (Iv (min a c) (max b d))

{- Note: stretching an interval: 
   -----------a---------b----c---------d  <- bad
   -----------a-----c------b-----------d
   -----------c-----a----b-------------d
   -----------c-----a----d-------------b
   -----------c-----d-----a------------b  <- bad 
----------------------------------------------------------------------}


{----------------------------------------------------------------------
  The Range
----------------------------------------------------------------------}
-- | Range
data Range 
  = RInt  (IVal Integer)   -- integers
  | RBool (IVal Bool)      -- booleans
  | RArr Interval Interval -- arrays
  | ROther                 -- other but also equivalent to bottom
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

instance Outputable Range where 
  ppr r = text (show r)

rjoin :: Range -> Range -> Range
rjoin (RInt iv1)  (RInt iv2)
  = RInt  (iv1 `iv_join_discrete` iv2)
rjoin (RBool iv1) (RBool iv2)
  = RBool (iv1 `iv_join_discrete` iv2)
rjoin (RArr r1 w1) (RArr r2 w2)
  = RArr (r1 `ijoin_apx` r2) (w1 `ijoin_pcs` w2)
rjoin _ _  = ROther

rangeTop :: Ty -> Range 
rangeTop (TInt {})   = RInt IUnknown
rangeTop (TArray {}) = RArr IUnknown IUnknown
rangeTop (TBool)     = RBool IUnknown
rangeTop _           = ROther


runop :: UnOp -> Range -> Range
-- | This is sensitive to the type checker
runop Neg (RInt i)       = RInt $ liftM negate i
runop Neg _              = ROther
runop ALength _          = RInt IUnknown
runop (Cast (TInt {})) _ = RInt IUnknown
runop (Cast _) _         = ROther
runop Not (RBool b)      = RBool $ liftM not b
runop Not _              = ROther
runop NatExp (RInt {})   = RInt IUnknown
runop NatExp _           = ROther
runop BwNeg (RInt {})    = RInt IUnknown
runop BwNeg _            = ROther 


rbinop :: BinOp -> Range -> Range -> Range
-- | This is sensitive to the type checker

-- Arithmetic binops
rbinop Add (RInt r1) (RInt r2) = RInt $ liftM2 (+) r1 r2
rbinop Sub _ _                 = RInt IUnknown
rbinop Mult  _r1 _r2 = RInt IUnknown
rbinop Div   _r1 _r2 = RInt IUnknown
rbinop Rem   _r1 _r2 = RInt IUnknown
rbinop Expon _r1 _r2 = RInt IUnknown

-- Other binops that can return integers
rbinop ShL _r1 _r2   = RInt IUnknown
rbinop ShR _r1 _r2   = RInt IUnknown
rbinop BwAnd (RInt {}) _ = RInt IUnknown
rbinop BwOr (RInt {}) _  = RInt IUnknown
rbinop BwXor (RInt {}) _ = RInt IUnknown

-- Booleans 
rbinop Eq  (RInt i1) (RInt i2) = RBool $ liftM2 (==) i1 i2
rbinop Neq (RInt i1) (RInt i2) 
  = RBool $ liftM2 (\x y -> not (x == y)) i1 i2
rbinop Lt  (RInt i1) (RInt i2) = RBool $ liftM2 (<)  i1 i2
rbinop Gt  (RInt i1) (RInt i2) = RBool $ liftM2 (>)  i1 i2
rbinop Leq (RInt i1) (RInt i2) = RBool $ liftM2 (<=) i1 i2
rbinop Geq (RInt i1) (RInt i2) = RBool $ liftM2 (>=) i1 i2
rbinop Or _ _     = RBool IUnknown
rbinop And _ _    = RBool IUnknown
rbinop Eq  _ _    = RBool IUnknown
rbinop Neq _ _    = RBool IUnknown
rbinop Lt  _ _    = RBool IUnknown
rbinop Gt  _ _    = RBool IUnknown
rbinop Leq _ _    = RBool IUnknown
rbinop Geq _ _    = RBool IUnknown

-- Default cases 
rbinop Add _ _    = ROther
rbinop BwAnd _ _  = ROther
rbinop BwOr _ _   = ROther
rbinop BwXor _ _  = ROther


{----------------------------------------------------------------------
  Abstract domain for values
----------------------------------------------------------------------}

instance ValDom Range where
  aVal (VInt i)      = RInt (return i)
  aVal (VBool b)     = RBool (return b)
  aArr _vs           = RArr IUnknown IUnknown
  aStruct _ _        = ROther
  aUnOp uop rs       = runop uop rs
  aBinOp bop rsa rsb = rbinop bop rsa rsb

  aArrRead ret_ty _arr _ _ = rangeTop ret_ty
  aStrProj ret_ty _str _   = rangeTop ret_ty
 
{--------------------------------------------------------------------
  Abstract interpreter for commands
---------------------------------------------------------------------}

type RngMap = NameMap Ty Range

instance POrd Integer where 
 i1 `pleq` i2 = i1 <= i2

instance POrd v => POrd (IVal v) where
  _           `pleq` IUnknown    = True
  (IKnown v1) `pleq` (IKnown v2) = v1 `pleq` v2

instance POrd Iv where 
  (Iv i1 i2) `pleq` (Iv j1 j2) = (i1 `pleq` j1) && (i2 `pleq` j2)

instance POrd Range where 
  RInt intv1   `pleq` RInt intv2   = intv1 `pleq` intv2
  ROther       `pleq` ROther       = True
  RArr ri1 wi1 `pleq` RArr ri2 wi2 = ri1 `pleq` ri2 && wi1 `pleq` wi2
  ROther       `pleq` _            = True 

joinRngMap :: RngMap -> RngMap -> RngMap
joinRngMap rm1 rm2 
  = neUnionWithMb rm1 rm2 (\_ mbr1 r2 -> rjoin (aux mbr1) r2)
  where aux Nothing  = ROther
        aux (Just r) = r

newtype Rng a = Rng (StateT RngMap (ErrorT Doc IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState RngMap
           , MonadError Doc
           , MonadIO
           )

instance AbsInt Rng Range where 
  aSkip = return ROther

  aJoin m1 m2 = do

    (v1, post1) <- inCurrSt m1
    (v2, post2) <- inCurrSt m2

    let post = joinRngMap post1 post2

{- Debugging: 
    liftIO $ print $
             vcat [ text "aJoin"
                  , nest 2 $
                    vcat [ text "post1 = "
                         , ppr post1
                         , text "~~~~"
                         , text "post2 = "
                         , ppr post2
                         ]
                  , nest 2 $
                    vcat [ text "joined post = "
                         , ppr post 
                         ]
                  ]
-}

    put post
    return (v1 `rjoin` v2)

  aWithFact (RBool (IKnown True))  action = action
  aWithFact (RBool (IKnown False)) action = aSkip
  aWithFact _ action                      = action


varSetRng :: EId -> Range -> Rng Range
varSetRng x range = do
  s <- get
  put $ neUpdate x upd s
  return range
  where upd _ = range

varGetRng :: EId -> Rng (Maybe Range)
varGetRng x = get >>= (return . neLookup x)

derefVarRead :: EId -> Rng Range
derefVarRead x = do 

updArrRdRng :: EId -> Interval -> Maybe Range -> Rng Range
updArrRdRng x ival Nothing =
  let new_range = RArr ival IvEmpty
  in varSetRng x new_range >> return new_range
updArrRdRng x ival (Just (RArr riv wiv)) = 
  let new_range = RArr (istretch False riv ival) wiv 
  in varSetRng x new_range >> return new_range
updArrRdRng x ival (Just rother) = return rother

updArrWrRng :: EId -> Interval -> Maybe Range -> Rng ()
updArrWrRng x ival Nothing = varSetRng $ RArr IvEmpty ival
updArrWrRng x ival (Just (RArr riv wiv))
  = varSetRng x $ RArr riv (istretch True wiv ival)
updArrWrRng x ival (Just rother) = return ()

derefVarRead :: EId -> Rng Range
derefVarRead x = do 
  case nameType x of 
    TInt {}  -> maybe (RInt IUnknown) id  <$> varGetRng x
    TBool {} -> maybe (RBool IUnknown) id <$> varGetRng x
    TArray (Literal n) _ -> varGetRng x >>= updArrRdRng x (IKnown (Iv 0 n))
    TArray _ _           -> varGetRng x >>= updArrRdRng x IUnknown 
     _other -> maybe ROther id <$> varGetRng x 

derefVarWrite :: EId -> Range -> Rng ()
derefVarWrite x rng = do 
  case nameType x of
    TInt {}  -> setVarRng x rng
    TBool {} -> setVarRng x rng
    TArray (Literal n) _ -> varGetRng x >>= updArrWrRng x (IKnown (Iv 0 n))
    TArray _ _           -> varGetRng x >>= updArrWrRng x IUnknown 
     _other -> return ()

sliceToInterval :: Range -> LengthInfo -> Interval
sliceToInterval ROther _ = IUnknown
sliceToInterval (RInt IUnknown) _ = IUnknown
sliceToInterval (RInt (IKnown i)) (LILength j)  = IKnown (Iv i (i+j-1))
sliceToInterval (RInt (IKnown i)) (LISingleton) = IKnown (Iv i i)
sliceToInterval _ _ = IUnknown

instance CmdDom Rng RngVal where

  aDerefRead lval 
     = let dbg = text $ "aDerefRead, lval = " ++ show lval
       in dbgRngSt dbg (go lval)
     where go d
             | GDVar x <- d = derefVarRead x
             | GDArr (GDVar x) ridx linfo <- d
             , let rdival = sliceToInterval ridx linfo
             = updArrRdRng x ival =<< varGetRng x
             | GDArr d' _ _ <- d = go d' >> rangeTop (ctDerefExp d)
             | GDProj d' _ <- d  = go d' >> rangeTop (ctDerefExp d)
             | otherwise         = error "aDerefRead"

  aAssign lval r 
     = let dbg = vcat [ text $ "aAssign, lval= " ++ show lval
                      , text $ "rhs range, r = " ++ show r 
                      ]
       in dbgRngSt dbg (go lval r)
     where go d r 
              | GDVar x <- d = derefVarWrite x r
              | otherwise    = effects d 
           effects d
              | GDArr (GDVar x) ridx linfo <- d
              , let wdival = sliceToInterval ridx linfo
              = updArrWrRng x wdival =<< varGetRng x
              | GDArr d' _ _ <- d = effects d'
              | GDProj d' _ <- d  = effects d'
              | otherwise = error "aAssign"

  withImmABind nm rng action 
    = do pre <- get
         put $ neExtend nm (av_range rng) pre
         res <- action
         post <- get
         -- Delete variable from state
         put $ neUpdate nm (\_ -> Nothing) post
         return res

  withMutABind nm action 
    = do res <- action
         post <- get 
         -- Delete nm from state
         put $ neUpdate nm (\_ -> Nothing) post
         return res

  aCall  _ _ = fail "Calls not supported in range analysis"
  aError     = fail "Error not supported in range analysis"
  aPrint _ _ = return ()




{------------------------------------------------------------------------
  Running the analysis
------------------------------------------------------------------------}
deriving instance MonadState RngMap (AbsT Rng) 
deriving instance Monad (AbsT Rng)

pprRanges :: RngMap -> Doc
pprRanges r = vcat $
  map (\(k,v) -> ppr k <> char ':' <+> text (show v)) (neToList r)


runRng :: Rng a -> ErrorT Doc IO (a, RngMap)
runRng (Rng action) = runStateT action neEmpty

varRanges :: DynFlags -> Exp -> ErrorT Doc IO (RngMap, Range)
varRanges _dfs e =
  case action of
    AbsT m -> do
      (rngval, rmap) <- runRng m
      return (rmap, av_range rngval)
  where 
    action :: AbsT Rng Range
    action = absEvalRVal e
