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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-orphans #-}

-- | Range analysis
module Ziria.Analysis.RangeAnal (
   Range(..)
 , Interval
 , IVal(..)
 , Iv(..)
 , RngMap
 , pprRanges
 , varRanges
) where

import Control.Applicative
import Control.DeepSeq.Generics (NFData(..), genericRnf)
import Control.Monad.Error
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJ
import Text.Show.Pretty ()

import Ziria.Analysis.AbsInt
import Ziria.BasicTypes.AstExpr
import Ziria.BasicTypes.NameEnv
import Ziria.BasicTypes.Outputable 
import Ziria.BasicTypes.PpExpr ()
import Ziria.ComputeType.CtExpr
import Ziria.Utils.Orphans ()

import Opts


{- Note [Range Analysis] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The goal of the range analysis is, given an expression, to identify a
map from its free array variables to:

  a) an overapproximation of the input range
  b) a precise interval for the output range

We use (a) to construct a "tighter" input space for LUT generation
than the default which uses the whole bitwidth of input arrays. If (b)
determines that we write to the full array range then we do not have
to use LUT output masks for this variable; instead we can directly
read it off from the LUT entry.

NB: for the time being the treatment of structs (and arrays of
structs) is conservative (TODO: improve)

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

-- Boilerplate
instance Functor IVal where
    fmap f x = x >>= return . f
instance Applicative IVal where
    pure   = return
    (<*>)  = ap


iv_join_discrete :: Eq v => IVal v -> IVal v -> IVal v
iv_join_discrete IUnknown _ = IUnknown
iv_join_discrete _ IUnknown = IUnknown
iv_join_discrete (IKnown v1) (IKnown v2)
  | v1 == v2  = IKnown v1
  | otherwise = IUnknown

rint_widen :: IVal Integer -> IVal Integer
-- Use a small range [0,256] to make the whole 
-- thing insensitive to signedness
rint_widen IUnknown = IUnknown
rint_widen (IKnown i) | i >= 256  = IUnknown
                      | i < 0     = IUnknown
                      | otherwise = IKnown i 

-- | Intervals
data Iv = Iv Integer Integer  -- i1 <= i2
        | IvEmpty
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

ivEmpty :: IVal Iv
ivEmpty = IKnown IvEmpty

ivIv :: Integral a => a -> a -> IVal Iv
ivIv i j = IKnown (Iv (fromIntegral i)(fromIntegral j))

ivUnknown :: IVal Iv
ivUnknown = IUnknown


instance NFData Iv where
  rnf = genericRnf

type Interval = IVal Iv

instance NFData Interval where
  rnf = genericRnf

-- | Used to find an over approximation of the interval
ijoin :: Interval -> Interval -> Interval 
ijoin IUnknown _ = IUnknown
ijoin _ IUnknown = IUnknown
ijoin (IKnown (Iv i j)) (IKnown (Iv i' j')) 
  | let l = min i i'
  , let h = max j j'
  , let bound = 512
  = if l < - bound || h > bound then IUnknown else ivIv l h
ijoin (IKnown iv) (IKnown IvEmpty) = IKnown iv
ijoin (IKnown IvEmpty) (IKnown iv) = IKnown iv

ijoin_pcs :: Interval -> Interval -> Interval
ijoin_pcs IUnknown _ = IUnknown
ijoin_pcs _ IUnknown = IUnknown
ijoin_pcs (IKnown (Iv i j)) (IKnown (Iv i' j'))
  | let l = min i i'
  , let h = max j j'
  , let bound = 512
  , let gap = (i - j' > 1) || (i' - j > 1)
  = if l < - bound || h > bound || gap then IUnknown else ivIv l h
ijoin_pcs (IKnown iv) (IKnown IvEmpty) = IKnown iv
ijoin_pcs (IKnown IvEmpty) (IKnown iv) = IKnown iv


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
  = RInt  (IVal Integer)    -- integers
  | RBool (IVal Bool)       -- booleans
  | RArr Interval Interval  -- arrays
  | ROther                  -- other but also equivalent to bottom
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

-- Don't even construct integers above a certain threshold
mkRInt :: IVal Integer -> Range
mkRInt iv = RInt $ rint_widen iv

-- TODO:
-- We should add structs here in the future:
--  ... | RStruct Ty (IVal [FldName]) (IVal [FldName]
-- and treat it similarly to RArr. 
-- --------------------------------------------------

instance Outputable Range where 
  ppr r = text (show r)

rjoin :: Range -> Range -> Range
rjoin (RInt iv1)  (RInt iv2)
  = mkRInt  (iv1 `iv_join_discrete` iv2)
rjoin (RBool iv1) (RBool iv2)
  = RBool (iv1 `iv_join_discrete` iv2)
rjoin (RArr r1 w1) (RArr r2 w2)
  = RArr (r1 `ijoin` r2) (w1 `ijoin_pcs` w2)
rjoin _ _  = ROther

rangeTop :: Ty -> Range 
rangeTop (TInt {})   = mkRInt IUnknown
rangeTop (TArray {}) = RArr IUnknown IUnknown
rangeTop (TBool)     = RBool IUnknown
rangeTop _           = ROther


runop :: UnOp -> Range -> Range
-- | This is sensitive to the type checker
runop Neg (RInt i)       = mkRInt $ liftM negate i
runop Neg _              = ROther
runop ALength _          = mkRInt IUnknown
runop (Cast (TInt {})) _ = mkRInt IUnknown
runop (Cast _) _         = ROther
runop Not (RBool b)      = RBool $ liftM not b
runop Not _              = ROther
runop NatExp (RInt {})   = mkRInt IUnknown
runop NatExp _           = ROther
runop BwNeg (RInt {})    = mkRInt IUnknown
runop BwNeg _            = ROther 


rbinop :: BinOp -> Range -> Range -> Range
-- | This is sensitive to the type checker

-- Arithmetic binops
rbinop Add (RInt r1) (RInt r2) = mkRInt $ liftM2 (+) r1 r2
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
  aVal (VInt i _)    = mkRInt (return i)
  aVal (VBool b)     = RBool (return b)
  aVal _             = ROther
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

instance POrd Bool where 
  b1 `pleq` b2 = b1 == b2

instance POrd v => POrd (IVal v) where
  _           `pleq` IUnknown    = True
  (IKnown v1) `pleq` (IKnown v2) = v1 `pleq` v2
  _           `pleq` _           = False

instance POrd Iv where 
  (Iv i1 i2)  `pleq` (Iv j1 j2) = (i1 `pleq` j1) && (i2 `pleq` j2)
  IvEmpty     `pleq` _          = True
  Iv i j      `pleq` IvEmpty    = (i < j)


instance POrd Range where 
  RInt intv1   `pleq` RInt intv2   = intv1 `pleq` intv2
  RBool b1     `pleq` RBool b2     = b1 `pleq` b2
  ROther       `pleq` ROther       = True
  RArr ri1 wi1 `pleq` RArr ri2 wi2 = ri1 `pleq` ri2 && wi1 `pleq` wi2
  _            `pleq` ROther       = True
  _            `pleq` _            = False 

joinRngMap :: RngMap -> RngMap -> RngMap
joinRngMap rm1 rm2 
  = neJoinWith rm1 rm2 (\_ r -> rjoin ROther r) 
                       (\_ r1 r2 -> rjoin r1 r2)

newtype Rng a = Rng (StateT RngMap (ErrorT Doc IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState RngMap
           , MonadError Doc
           , MonadIO
           )

dbgRange :: Rng ()
dbgRange = return () 
  -- get >>= \x -> liftIO $ putStrLn (render (pprRanges x))


instance AbsInt Rng Range where 

  aTrace = dbgRange

  aSkip = return ROther
  aWiden (RBool (IKnown True)) m1 _m2  = m1
  aWiden (RBool (IKnown False)) _m1 m2 = m2
  aWiden _ m1 m2 = aJoin m1 m2
  
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

  aWithFact (RBool (IKnown True))  action  = action
  aWithFact (RBool (IKnown False)) _action = aSkip
  aWithFact _ action                       = action


 
varSetRng :: EId -> Range -> Rng ()
varSetRng x range = do
  s <- get
  put $ neUpdate x (\_ -> Just range) s

varGetRng :: EId -> Rng (Maybe Range)
varGetRng x = neLookup x <$> get 


updArrRdRng :: EId -> Interval -> Maybe Range -> Rng Range
updArrRdRng x ival Nothing =
  let new_range = RArr ival ivEmpty
  in varSetRng x new_range >> return new_range
updArrRdRng x ival (Just (RArr riv wiv)) = 
  let new_range = RArr (ijoin riv ival) wiv 
  in varSetRng x new_range >> return new_range
updArrRdRng _x _ival (Just rother) = return rother

updArrWrRng :: EId -> Interval -> Maybe Range -> Rng ()
updArrWrRng x ival Nothing = varSetRng x $ RArr ivEmpty ival
updArrWrRng x ival (Just (RArr riv wiv))
  = varSetRng x $ RArr riv (ijoin_pcs wiv ival)
updArrWrRng _x _ival (Just _rother) = return ()


derefVarRead :: EId -> Rng Range
derefVarRead x = do 
  case nameTyp x of 
    TInt  {} -> maybe (RInt IUnknown) id  <$> varGetRng x
    TBool {} -> maybe (RBool IUnknown) id <$> varGetRng x
    TArray (Literal n) _ -> updArrRdRng x (ivIv 0 n)  =<< varGetRng x 
    TArray _ _           -> updArrRdRng x (ivUnknown) =<< varGetRng x
    _other               -> maybe ROther id <$> varGetRng x

derefVarWrite :: EId -> Range -> Rng ()
derefVarWrite x rng = do 
  case nameTyp x of
    TInt {}  -> varSetRng x rng
    TBool {} -> varSetRng x rng
    TArray (Literal n) _ -> varGetRng x >>= updArrWrRng x (ivIv 0 n)
    TArray _ _           -> varGetRng x >>= updArrWrRng x ivUnknown 
    _other                -> return ()


sliceToInterval :: Range -> LengthInfo -> Interval
sliceToInterval ROther _ = IUnknown
sliceToInterval (RInt IUnknown) _ = IUnknown
sliceToInterval (RInt (IKnown i)) (LILength j) = ivIv i (i + fromIntegral (j-1))
sliceToInterval (RInt (IKnown i)) (LISingleton) = ivIv i i
sliceToInterval _ _ = IUnknown

instance CmdDom Rng Range where

  aDerefRead lval = go lval
   where 
     go d
       | GDVar x <- d = derefVarRead x
       | GDArr (GDVar x) ridx linfo <- d
       , let rdival = sliceToInterval ridx linfo
       = updArrRdRng x rdival =<< varGetRng x
       | GDArr d' _ _ <- d = do { _ <- go d'; return $ rangeTop (ctDerefExp d)}
       | GDProj d' _ <- d  = do { _ <- go d'; return $ rangeTop (ctDerefExp d)}
       | otherwise         = error "aDerefRead"

  aAssign lval_lhs rng_rhs = go lval_lhs rng_rhs
   where
     go d r 
        | GDVar x <- d = derefVarWrite x r
        | GDArr (GDVar x) ridx linfo <- d
        , let wrival = sliceToInterval ridx linfo
        = updArrWrRng x wrival =<< varGetRng x
        | otherwise    = effects_top d 

     -- If the lval is not of the form x or x[i] then our Range cannot
     -- at the moment represent things like x[i].f so we won't get a
     -- precise range and it'd be erroneous to consider that we write
     -- in the full range of x (there may be another frield x[i].g,
     -- never written too, for instance).
     -- 
     -- The cost of this imprecision is that we will be using a
     -- dynamically generated mask in the LUT entry. Once we improve
     -- precision we may get rid of more dynamic masks and improve
     -- performance.

     effects_top d
        | GDArr d' _ _ <- d = effects_top d'
        | GDProj d' _ <- d  = effects_top d'
        | GDVar x <- d      = varSetRng x (rangeTop (nameTyp x))
        | otherwise         = return ()

  withImmABind nm rng action 
    = do pre <- get
         put $ neExtend nm rng pre
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

varRanges :: DynFlags -> Exp -> ErrorT Doc IO (Range, RngMap)
varRanges _dfs e =
  case action of AbsT m -> runRng m
  where 
    action :: AbsT Rng Range
    action = absEvalRVal e

