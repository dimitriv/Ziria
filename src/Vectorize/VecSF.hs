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
module VecSF where

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

import Opts

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

We express the aforementioned vectorization plans with the Scaling
Factor datatypes.

-}

{-------------------------------------------------------------------------------
  Scaling factors
-------------------------------------------------------------------------------}

data SFUnknown = SFUnknown { sf_mults :: [Int] }
data SFKnown = SFKnown {
     sf_other       :: Int
   , sf_other_divs  :: [(Int,Int)]
   , sf_other_mults :: [(Int,Int)] }

-- | Scaling factor data types 
data SFUD -- | UD scale factors
   = SFUD { sfud_in  :: Int, sfud_out :: Either SFKnown SFUnknown }
data SFDU -- | DU scale factors
   = SFDU { sfdu_out :: Int, sfdu_in  :: Either SFKnown SFUnknown }
data SFDD -- | DD scale factors
   = SFDD_In    { sfdd_in  :: SFKnown }
   | SFDD_Out   { sfdd_out :: SFKnown }
   | SFDD_InOut { sfdd_in  :: SFKnown, sfdd_out :: SFKnown }

-- | The context CtxForVect keeps track on whether there is a computer
-- connected to our input queue or to our output queue (transitively)
data CtxForVect 
  = -- Unrestricted context (e.g top-level)
    CtxUnrestricted
    -- There exists a computer to the left of current component, 
    -- i.e. we could be a T-after-C. 
  | CtxExistsCompLeft
    -- There exists a computer to the right of current component, 
    -- i.e. we could be a T-before-C
  | CtxExistsCompRight

-- | Compute SFUD scale factor
compSFUD :: (CAlpha,CAlpha) -> Maybe SFUD
compSFUD (CAUnknown,_)  = Nothing 
compSFUD (CAMultOf _,_) = Nothing
compSFUD (CAStatic i,CAStatic j)
  = return $ SFUD { sfud_in = i, sfud_out = Left (compSFKnown j) }
compSFUD (CAStatic i,_)
  = return $ SFUD { sfud_in = i, sfud_out = Right compSFUnknown  }

-- | Compute SFDU scale factor
compSFDU :: (CAlpha,CAlpha) -> Maybe SFDU
compSFDU (_,CAUnknown)  = Nothing
compSFDU (_,CAMultOf _) = Nothing
compSFDU (CAStatic i, CAStatic j)
  = return $ SFDU { sfdu_out = j, sfdu_in = Left (compSFKnown i) }
compSFDU (_,CAStatic j)
  = return $ SFDU { sfdu_out = j, sfdu_in = Right compSFUnknown  }

-- | Compute SFDD scale factor
compSFDD :: (CAlpha,CAlpha) -> Maybe SFDD
compSFDD (CAStatic i,CAStatic j)
  = return $ SFDD_InOut { sfdd_in = compSFKnown i, sfdd_out = compSFKnown j }
compSFDD (CAStatic i,_)
  = return $ SFDD_In { sfdd_in = compSFKnown i }
compSFDD (_,CAStatic j)
  = return $ SFDD_Out { sfdd_out = compSFKnown j }
compSFDD (_,_) 
  = Nothing

compSFKnown :: Int -> SFKnown
compSFKnown i
  = SFKnown {
       sf_other       = i
     , sf_other_divs  = divs_of i
     , sf_other_mults 
           -- Artificially restrict the search space to sensible arrays
         = filter (\(m1,m2) -> m1*m2*i <= vECT_ARRAY_BOUND) $
           multsUpTo vECT_MULT_BOUND }
  where 
    divs_of n = if n > 0 then divsOf n else []

compSFUnknown :: SFUnknown 
compSFUnknown = SFUnknown [1..vECT_MULT_BOUND]

-- | Bound for multiplicities of vectorization
vECT_MULT_BOUND  = 128 

-- | Bound for input/output array sizes
vECT_ARRAY_BOUND = 288

-- | Pairs that multiply to a number not higher than the bound
multsUpTo :: Int -> [(Int,Int)]
multsUpTo bnd = [ (z1,z2) | x <- [1..bnd], (z1,z2) <- divsOf x
                          , z1 > 1 || z2 > 1 ]

-- | Divisors of a number
divsOf :: Int -> [(Int,Int)]
divsOf n = [ (x,y) | x <- [1..n], y <- [1..n], x*y == n ]
