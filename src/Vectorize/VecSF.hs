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
module VecSF (
    SFDU (..), compSFDU
  , SFUD (..), compSFUD
  , SFDD (..), compSFDD
  , vECT_MULT_BOUND
  , vECT_IOARRAY_BOUND
  , CtxForVect (..)

  , sfud_arity
  , sfdu_arity
  , sfdd_arity
  , isSteady_sfdu
  , isSteady_sfud

  , NMul (..), NDiv (..), DivsOf (..)

  , divsOfMemo
) where

import AstExpr
import AstComp
import AstFM

import PpComp
import Outputable
import qualified GenSym as GS
import Text.Parsec.Pos
import qualified Data.Set as S
import Control.Monad.State
import Data.List as M
import Data.Functor.Identity

import Data.IORef

import qualified Data.Map  as Map
import Opts


import System.IO.Unsafe ( unsafePerformIO ) 

import VecM ( isVectorizable )

import CardAnalysis

{- Note [Vectorization Modes] 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~

The vectorization outline is described in the ASPLOS'15 paper. Here
we give some further implementation details. We use notation

     a^ain -> b^aout

for a component with input and output types a and b respectively, and
input and output cardinalities ain and aout respectively. The return
types in the case of computers are irrelevant for vectorization, so we
don't even write them here.

The vectorizer supports several different rewrites of /simple/
computers.  Reminder: a simple computer is a computer, that is having
a type (ST (C ..) .. ..), with no nested uses of >>>.

[UD1] a^i       -> b^j       ~~~> (a*i*m1*m2)   -> (b*j*m1)^m2 
[UD2] a^i       -> b^(j0*j1) ~~~> (a*i*m)       -> (b*j0)^(j1*m)
[UD3] a^i       -> X         ~~~> (a*i*m)       -> X^m

[DU1] a^i       -> b^j       ~~~> (a*i*m1)^m2   -> (b*j*m1*m2)
[DU2] a^(i0*i1) -> b^j       ~~~> (a*i0)^(i1*m) -> (b*j*m)
[DU3] X         -> b^j       ~~~> X^m           -> (b*j*m)

[DD1] a^(i*j)   -> X         ~~~> (a*i)^j       -> X
[DD2] X         -> b^(i*j)   ~~~> x             -> (b*i)^j
[DD3] a^(i0*i1) -> b^(j0*j1) ~~~> (a*i0)^i1     -> (b*j0)^j1

where above we use letter X for a CAlpha that is not a CAStatic, 
i.e. currently we make no use of the CAMult cardinality either.

Why the multitude of all those rules? The idea is that the rules above
cover _all_ cases of vectorizations to divisors and multiples of the
cardinalities, and moreover both in the input queue and in the output
queue.

For reasons explained in the ASPLOS paper, not all rules are applicable. 
Namely the following restrictions hold:

Computer                    : can use only DD1, DD2, DD3 
*Transformer-before-computer: can use only DU1, DU2, DU3, DD1, DD2, DD3 
*Transformer-after-computer : can use only UD1, UD2, UD3, DD1, DD2, DD3
*Transformer-in-isolation   : all apply(^)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(*) By Transformer- above we really mean rewriting a simple computer `c' 
    in a `repeat c' transformer -- we call those 'simple transformers'
(^) ... But are there any such transformers in our pipelines, actually?


The UD* and DU* rules are implemented in VecScaleUp.hs
The DD*         rules are implemented in VecScaleDn.hs

We express the aforementioned vectorization plans with 
the Scaling Factor datatypes.

-}


{-------------------------------------------------------------------------------
  Vectorization context
-------------------------------------------------------------------------------}


-- | CtxForVect keeps track on whether there is a computer connected
-- to our input queue or to our output queue. This determines which
-- vectorization mode is applicable.
data CtxForVect 
  = 
    -- Unrestricted context, UD, DU, DD all apply
    CtxUnrestricted

    -- T-after-C, appliable modes are: UD, DD 
  | CtxExCompLeft

    -- T-before-C, applicable modes are: DU, DD
  | CtxExCompRight

    --  c >>> do { t >>> c1 
    --           ; t' }
    -- In this case we can only use DD on t.
  | CtxExCompLeftAndRight
  deriving Show

{-------------------------------------------------------------------------------
  Scaling factors
-------------------------------------------------------------------------------}

-- | Data structure capturing a number and two divisors of it
-- Invariant: divs_of = d0_div * d1_div
data DivsOf = 
  DivsOf { divs_of :: Int, d0_div  :: NDiv, d1_div  :: NDiv }
  deriving Show
-- | Just a newtype to avoid confusing ints with multiplicities or divisors
newtype NMul = NMul { unNMul :: Int }
  deriving Show
newtype NDiv = NDiv { unNDiv :: Int }
  deriving Show

divsOfMemo :: IORef (Map.Map Int [DivsOf])
divsOfMemo = unsafePerformIO (newIORef (Map.empty))

-- | Divisors of a number
divsOf :: Int -> [DivsOf]
divsOf n = unsafePerformIO $ do
   dm <- readIORef divsOfMemo
   case Map.lookup n dm of 
     Nothing -> do
       let ds' = [ DivsOf n (NDiv x) (NDiv y)
                 | x <- [1..n]
                 , y <- [1..n]
                 , x > 1 || y > 1
                 , x*y == n
                 ]
       writeIORef divsOfMemo (Map.insert n ds' dm)
       return ds'
     Just ds -> return ds

-- | All multiplicities
mults :: [NMul]
mults = map NMul [1..vECT_MULT_BOUND]

-- | Bound for multiplicities of vectorization
vECT_MULT_BOUND :: Int
vECT_MULT_BOUND  = 512

-- | Bound for input/output array sizes
vECT_IOARRAY_BOUND :: Int 
vECT_IOARRAY_BOUND = 512

-- | Bound for vectorizer-generated array sizes
vECT_ARRAY_BOUND :: Int
vECT_ARRAY_BOUND = 288

{-------------------------------------------------------------------------------
  SFUD scaling factors
-------------------------------------------------------------------------------}

-- | SFUD factors
data SFUD =
    SFUD1 Int    -- i 
          Int    -- j
          NMul   -- m1
          NMul   -- m2          
  | SFUD2 Int    -- i 
          DivsOf -- j,j0,j1: j0*j1 == j
          NMul   -- m
  | SFUD3 Int    -- i
          NMul   -- m
  deriving Show

-- | Compute SFUD scale factor
compSFUD_aux :: (CAlpha,CAlpha) -> [SFUD]
compSFUD_aux (CAUnknown,_)  = []
compSFUD_aux (CAMultOf _,_) = []
compSFUD_aux (CAStatic i, CAStatic j) = ud1s ++ ud2s
  where 
    ud1s = [ SFUD1 i j (NMul m1) (NMul m2)
           | NMul m <- mults
           , DivsOf _ (NDiv m1) (NDiv m2) <- divsOf m 
           , i*m1*m2 <= vECT_ARRAY_BOUND
           , j*m1    <= vECT_ARRAY_BOUND
           ]
    ud2s 
      | j > 1  -- rewriting possible in the output
      = [ SFUD2 i d (NMul m)
           | d <- divsOf j
           , NMul m <- mults 
           , m*j <= vECT_ARRAY_BOUND
        ]
      | otherwise
      = compSFUD_aux (CAStatic i, CAUnknown) 

compSFUD_aux (CAStatic i, _) 
  = [ SFUD3 i (NMul m)
    | (NMul m) <- mults 
    , i*m <= vECT_ARRAY_BOUND
    ]

{-------------------------------------------------------------------------------
  SFDU scaling factors
-------------------------------------------------------------------------------}

-- | SFDU factors
data SFDU =
    SFDU1 Int    -- i
          Int    -- j
          NMul   -- m1
          NMul   -- m2
  | SFDU2 DivsOf -- i,i0,i1: i0*i1 == i
          Int    -- j
          NMul   -- m
  | SFDU3 Int    -- j
          NMul   -- m
  deriving Show 

-- | Compute SFDU scale factor (by exploiting symmetry)
-- to avoid having to re-implement the delicate bound checking
compSFDU_aux :: (CAlpha,CAlpha) -> [SFDU]
compSFDU_aux (ca1,ca2) = map sym_sfud $ compSFUD_aux (ca2,ca1)

sym_sfud (SFUD1 i j m1 m2) = SFDU1 j i m1 m2
sym_sfud (SFUD2 i ds m)    = SFDU2 ds i m
sym_sfud (SFUD3 i m)       = SFDU3 i m

-- | When is an UD/DU scale factor steady i.e. does not increase nor
-- decrease the rate of a component? We need those for vectorization
-- of Repeat nodes in CtxExCompLeftAndRight mode.
isSteady_sfdu :: SFDU -> Bool 
isSteady_sfdu (SFDU1 i j (NMul m1) (NMul m2)) 
 | m2 == 1 = True
isSteady_sfdu (SFDU2 (DivsOf i (NDiv i0) (NDiv i1)) j (NMul m))
 | i1*m == 1 = True
isSteady_sfdu (SFDU3 j (NMul m)) 
 | m == 1 = True
isSteady_sfdu _ = False

isSteady_sfud :: SFUD -> Bool
isSteady_sfud = isSteady_sfdu . sym_sfud



{-------------------------------------------------------------------------------
  SFDD scaling factors
-------------------------------------------------------------------------------}

-- | SFDD factors
-- NB: it will return some factors *only* if some rewriting is needed
data SFDD =
    SFDD0 { sfdd_i :: Int, sfdd_j :: Int } -- i <= 1, j <= 1
  | SFDD1 { sfdd_in  :: DivsOf } -- i,i0,i1, i0*i1 == i  (i >= 1)
  | SFDD2 { sfdd_out :: DivsOf } -- j,j0,j1, j0*j1 == j  (j >= 1)
  | SFDD3 { sfdd_in  :: DivsOf, sfdd_out :: DivsOf }  -- (i >= 1 && j >= 1)
  deriving Show

-- | Compute SFDD scale factor
compSFDD_aux :: (CAlpha,CAlpha) -> [SFDD]
compSFDD_aux (CAStatic i, CAStatic j)
  | i <= 1 && j <= 1 = [SFDD0 i j] -- nothing to rewrite at all
  | i <= 1 = compSFDD_aux (CAUnknown,CAStatic j)  -- j > 1
  | j <= 1 = compSFDD_aux (CAStatic i, CAUnknown) -- i > 1
  | otherwise -- both > 1
  = [ SFDD3 dsi dsj | dsi <- divsOf i, dsj <- divsOf j ]

compSFDD_aux (CAStatic i,_) | i > 1 = [ SFDD1 dsi | dsi <- divsOf i ]
compSFDD_aux (_,CAStatic j) | j > 1 = [ SFDD2 dsj | dsj <- divsOf j ]

compSFDD_aux _ = []


{-------------------------------------------------------------------------------
  Exported interface to compute scale factors
-------------------------------------------------------------------------------}

-- | Take cardinality info and compute a pair of two CAlphas
-- for the vectorizer to use when computing scale factors. 
normalize_card :: Card -> Ty -> Ty -> [(CAlpha,CAlpha)]
normalize_card OCard ty1 ty2 = []
normalize_card (SCard ain aout) tin tout
  = normalize_card_aux ain' aout'
  where ain'  = normalize_alpha ain  tin
        aout' = normalize_alpha aout tout
        normalize_card_aux CAUnknown CAUnknown = []
        normalize_card_aux ca ca' = [(ca,ca')]

-- | Although we may have computed cardinality information for non
-- vectorizable types, we normalize it to CAUnknown so that the
-- vectorizer does not kick in.
normalize_alpha :: CAlpha -> Ty -> CAlpha 
normalize_alpha ca ty = if isVectorizable ty then ca else CAUnknown

compSFUD :: Card -> Ty -> Ty -> [SFUD]
compSFUD card tin tout = normalize_card card tin tout >>= compSFUD_aux
compSFDD :: Card -> Ty -> Ty -> [SFDD]
compSFDD card tin tout = normalize_card card tin tout >>= compSFDD_aux
compSFDU :: Card -> Ty -> Ty -> [SFDU]
compSFDU card tin tout = normalize_card card tin tout >>= compSFDU_aux



-- | Convert scale-factors to the arity (size) of the arrays we take or emit
sfud_arity :: SFUD -> (Maybe Int, Maybe Int)
sfud_arity (SFUD1 i j (NMul m1) (NMul m2)) = (Just (i*m1*m2), Just (j*m1))
sfud_arity (SFUD2 i (DivsOf j (NDiv j0) j1) (NMul m)) = (Just (i*m), Just j0)
sfud_arity (SFUD3 i (NMul m)) = (Just (i*m) , Nothing)

sfdu_arity :: SFDU -> (Maybe Int, Maybe Int)
sfdu_arity (SFDU1 i j (NMul m1) (NMul m2)) = (Just (i*m1), Just (j*m1*m2))
sfdu_arity (SFDU2 (DivsOf i (NDiv i0) i1) j (NMul m)) = (Just i0, Just (j*m))
sfdu_arity (SFDU3 j (NMul m)) = (Nothing, Just (j*m))

sfdd_arity :: SFDD -> (Maybe Int, Maybe Int) 
sfdd_arity (SFDD0 i j) = (Just i, Just j)
sfdd_arity (SFDD1 (DivsOf _ (NDiv i0) _)) = (Just i0, Nothing)
sfdd_arity (SFDD2 (DivsOf _ (NDiv j0) _)) = (Nothing, Just j0)
sfdd_arity (SFDD3 (DivsOf _ (NDiv i0) _) 
                  (DivsOf _ (NDiv j0) _)) = (Just i0, Just j0)



