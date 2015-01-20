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
{-# LANGUAGE ScopedTypeVariables #-}
module VecScaleUp ( doVectCompUD ) where

import AstExpr
import AstComp
import AstUnlabelled

import PpComp
import Outputable
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Monad.State

import Data.List as M

import CardAnalysis
import VecM
import VecSF

import Control.Applicative ( (<$>) )
import Control.Monad ( when )

import AstFM
import Opts

import CtComp
import CtExpr 

-- | UD driver
doVectCompUD :: DynFlags -> CTy -> LComp -> SFUD -> VecM DelayedVectRes
doVectCompUD dfs cty lcomp (SFUD1 i j m1 m2) 
  = vect_ud1 dfs cty lcomp i j m1 m2
doVectCompUD dfs cty lcomp (SFUD2 i (DivsOf j j0 j1) m) 
  = vect_ud2 dfs cty lcomp i j j0 j1 m
doVectCompUD dfs cty lcomp (SFUD3 i m) 
  = vect_ud3 dfs cty lcomp i m

zERO = I(0)

{-------------------------------------------------------------------------
[UD3] a^i -> X     ~~~>    (a*i*m) -> X^m
-------------------------------------------------------------------------}
vect_ud3 :: DynFlags -> CTy -> LComp -> Int -> NMul -> VecM DelayedVectRes
vect_ud3 dfs cty lcomp i (NMul m) = do
  venv <- getVecEnv
  zirc <- liftZr loc (zirbody venv)
  zirc_wrapped <- wrapCFunCall "_vect_ud2" loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty (yldTyOfCTy cty) minUtil }

  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    arin       = i*m

    -- NB: action does not rewrite emit(s)
    action venv iidx tidx xa
      = rwTakeEmitIO venv (rw_take arin iidx tidx xa)
                          (rw_takes iidx tidx xa) mEmit mEmits lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv
      | arin > 0  = ftake vin_ty >>= rest
      | otherwise = rest (error "BUG: Must not poke xa!") -- no take!
      where 
        rest xa
          = fleteref ("iidx" ::: tint ::= zERO) $ \iidx ->
            fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
            ftimes zERO m $ \_i ->
               ftimes zERO i $ const $
                 fembed (action venv iidx tidx xa)

{-------------------------------------------------------------------------
[UD2] a^i -> b^(j0*j1) ~~~> (a*i*m) -> (b*j0)^(j1*m)
-------------------------------------------------------------------------}
vect_ud2 :: DynFlags -> CTy -> LComp
         -> Int -> Int -> NDiv -> NDiv -> NMul -> VecM DelayedVectRes
vect_ud2 dfs cty lcomp i j (NDiv j0) (NDiv j1) (NMul m) = do
  venv <- getVecEnv
  zirc <- liftZr loc (zirbody venv)
  zirc_wrapped <- wrapCFunCall "_vect_ud2" loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty vout_ty minUtil }

  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m -- size of input array
    arout      = j0  -- size of output array

    action venv iidx oidx tidx xa ya 
      = rwTakeEmitIO venv (rw_take arin iidx tidx xa)
                          (rw_takes iidx tidx xa)
                          (rw_emit arout oidx tidx ya)
                          (rw_emits oidx tidx ya) lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv
      | arin > 0  = ftake vin_ty >>= rest
      | otherwise = rest (error "BUG: Must not poke xa!") -- no take!
      where 
        rest xa
          = fleteref ("iidx" ::: tint ::= zERO) $ \iidx ->
            fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
            fleteref ("ya"   ::: vout_ty) $ \ya ->
            ftimes zERO (j1*m) $ \_i ->
            fleteref ("oidx" ::: tint ::= I(0)) $ \oidx ->
              do { ftimes zERO j0 $ const $
                     fembed (action venv iidx oidx tidx xa ya)
                 ; when (arout > 1) $ do { oidx .:= zERO; femit ya }
                 }

{-------------------------------------------------------------------------
  [UD1] a^i -> b^j  ~~~> (a*i*m1*m2) -> (b*j*m1)^m2
-------------------------------------------------------------------------}
vect_ud1 :: DynFlags -> CTy -> LComp 
         -> Int -> Int -> NMul -> NMul -> VecM DelayedVectRes
vect_ud1 dfs cty lcomp i j (NMul m1) (NMul m2) = do
  venv <- getVecEnv
  zirc <- liftZr loc (zirbody venv)
  zirc_wrapped <- wrapCFunCall "_vect_ud1" loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty vout_ty minUtil }
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m1*m2 -- size of input array
    arout      = j*m1    -- size of output array

    action venv iidx oidx tidx xa ya
      = rwTakeEmitIO venv (rw_take arin iidx tidx xa)
                          (rw_takes iidx tidx xa)
                          (rw_emit arout oidx tidx ya)
                          (rw_emits oidx tidx ya) lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv
      | arin > 0  = ftake vin_ty >>= rest
      | otherwise = rest (error "BUG: Must not poke xa!") -- no take!
      where 
        rest xa
          = fleteref ("iidx" ::: tint ::= zERO) $ \iidx ->
            fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
            fleteref ("ya"   ::: vout_ty) $ \ya ->
            ftimes zERO m2 $ \_i ->
            fleteref ("oidx" ::: tint ::= I(0)) $ \oidx ->
              do { ftimes zERO m1 $ const $
                     fembed (action venv iidx oidx tidx xa ya)
                 ; when (arout > 1) $ do { oidx .:= zERO; femit ya }
                 }
