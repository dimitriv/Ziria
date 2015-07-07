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
module VecScaleUp ( doVectCompUD, doVectCompDU ) where

import AstExpr
import AstComp
import AstUnlabelled

import PpComp
import Outputable
import qualified GenSym as GS

import Data.Loc
import qualified Data.Set as S
import Control.Monad.State hiding ( (>=>) )

import Data.List as M

import CardAnalysis
import VecM
import VecSF

import VecRewriter

import Control.Applicative ( (<$>) )
import Control.Monad ( when )

import AstFM
import Opts

import CtComp
import CtExpr 

optzr :: Bool -> Zr v -> Zr v
optzr True  m  = m
optzr False _m = return (error "Illegal access!") 

embed = interpE noLoc

-- | UD driver
doVectCompUD :: DynFlags -> CTy -> LComp -> SFUD -> VecM DelayedVectRes
doVectCompUD dfs cty lcomp (SFUD1 i j m1 m2) 
  = vect_ud1 dfs cty lcomp i j m1 m2
doVectCompUD dfs cty lcomp (SFUD2 i (DivsOf j j0 j1) m) 
  = vect_ud2 dfs cty lcomp i j j0 j1 m
doVectCompUD dfs cty lcomp (SFUD3 i m) 
  = vect_ud3 dfs cty lcomp i m



{-------------------------------------------------------------------------
  [UD1] a^i -> b^j  ~~~> (a*i*m1*m2) -> (b*j*m1)^m2
-------------------------------------------------------------------------}
vect_ud1 :: DynFlags -> CTy -> LComp 
         -> Int -> Int -> NMul -> NMul -> VecM DelayedVectRes
vect_ud1 dfs cty lcomp i j (NMul m1) (NMul m2) 
  = mkDVRes zirbody "UD1" loc vin_ty vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m1*m2 -- size of input array
    arout      = j*m1    -- size of output array

    act venv st = rwTakeEmitIO venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = do
      -- if we do take > 1 then let's take upfront
      xa <- optzr (arin > 1) $ ftake vin_ty
      ftimes zERO m2 $ \c2 -> 
        do ya <- optzr (arout > 1) $ fneweref ("ya" ::: vout_ty)
           ftimes zERO m1 $ \c1 ->
              let offin  = embed (c2 .* i .* m1 .+ c1 .* i)
                  offout = embed (c1 .* j)
                  -- if arin  <= 1 then we won't rewrite input
                  -- if arout <= 1 (hence j*m1 <= 1) so just
                  -- emit every time (and do not emit in the end)
                  st = RwState { rws_in  = doRw arin xa offin
                               , rws_out = doRw arout ya offout }
              in fembed (act venv st)
           -- if arout > 1 then we were writing to an array instead
           -- of emitting so now it's time to emit
           when (arout > 1) (femit ya)

{-------------------------------------------------------------------------
[UD2] a^i -> b^(j0*j1) ~~~> (a*i*m) -> (b*j0)^(j1*m)
-------------------------------------------------------------------------}
vect_ud2 :: DynFlags -> CTy -> LComp
         -> Int -> Int -> NDiv -> NDiv -> NMul -> VecM DelayedVectRes
vect_ud2 dfs cty lcomp i j (NDiv j0) (NDiv j1) (NMul m)
  = mkDVRes zirbody "UD2" loc vin_ty (mkVectTy orig_outty j0)
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m -- size of input array
    arout      = j*m -- j > 1 because j0 and j1 are not /both/ 1

    act venv st = rwTakeEmitIO venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = 
      do { xa <- optzr (arin > 1)  $ ftake vin_ty
         ; ya <- fneweref ("ya" ::: vout_ty) 
         ; ftimes zERO m $ \cnt ->
              let offin  = embed (cnt .* i)
                  offout = embed (cnt .* j)
                  st = RwState { rws_in  = doRw arin xa offin
                               , rws_out = DoRw ya offout }
              in fembed (act venv st)
         ; femit ya
         } >=> fmitigate orig_outty arout j0
-- DV: here 
--      ftimes zERO (j1*m) $ \odx -> vectEmit ya odx j0
                    
{-------------------------------------------------------------------------
[UD3] a^i -> X     ~~~>    (a*i*m) -> X^m
-------------------------------------------------------------------------}
vect_ud3 :: DynFlags -> CTy -> LComp -> Int -> NMul -> VecM DelayedVectRes
vect_ud3 dfs cty lcomp i (NMul m) 
  = mkDVRes zirbody "UD3" loc vin_ty vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = yldTyOfCTy cty
    arin       = i*m

    act venv st = rwTakeEmitIO venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = do
      xa <- optzr (arin > 1) $ ftake vin_ty
      ftimes zERO m $ \cnt ->
         let offin = embed (cnt .* i)
             st    = RwState { rws_in = doRw arin xa offin, rws_out = DoNotRw }
         in fembed (act venv st)

{------------------------------------------------------------------------}

-- | DU driver
doVectCompDU :: DynFlags -> CTy -> LComp -> SFDU -> VecM DelayedVectRes
doVectCompDU dfs cty lcomp (SFDU1 i j m1 m2)
  = vect_du1 dfs cty lcomp i j m1 m2
doVectCompDU dfs cty lcomp (SFDU2 (DivsOf i i0 i1) j m)
  = vect_du2 dfs cty lcomp i i0 i1 j m
doVectCompDU dfs cty lcomp (SFDU3 j m)
  = vect_du3 dfs cty lcomp j m



{-------------------------------------------------------------------------
  [DU1] a^i -> b^j    ~~~>    (a*i*m1)^m2   -> (b*j*m1*m2)
-------------------------------------------------------------------------}
vect_du1 :: DynFlags -> CTy -> LComp
         -> Int -> Int -> NMul -> NMul -> VecM DelayedVectRes
vect_du1 dfs cty lcomp i j (NMul m1) (NMul m2)
  = mkDVRes zirbody "DU1" loc vin_ty vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m1    -- size of input array
    arout      = j*m1*m2 -- size of output array

    act venv st = rwTakeEmitIO venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = do
      ya <- optzr (arout > 1) $ fneweref ("ya" ::: vout_ty)
      ftimes zERO m2 $ \c2 -> 
        do xa <- optzr (arin > 1) $ ftake vin_ty
           ftimes zERO m1 $ \c1 ->
              let offin  = embed (c1 .* i)
                  offout = embed (c2 .* j .* m1 .+ c1 .* j)
                  st = RwState { rws_in  = doRw arin xa offin
                               , rws_out = doRw arout ya offout }
              in fembed (act venv st)
      when (arout > 1) $ femit ya

{-------------------------------------------------------------------------
[DU2] a^(i0*i1) -> b^j     ~~~>    (a*i0)^(i1*m) -> (b*j*m)
-------------------------------------------------------------------------}
vect_du2 :: DynFlags -> CTy -> LComp
         -> Int -> NDiv -> NDiv -> Int -> NMul -> VecM DelayedVectRes
vect_du2 dfs cty lcomp i (NDiv i0) (NDiv i1) j (NMul m) 
  = mkDVRes zirbody "DU2" loc (mkVectTy orig_inty i0) vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i*m -- i > 1 because i0 and i1 are not both 1!
    arout      = j*m -- size of output array

    act venv st = rwTakeEmitIO  venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = 
      fmitigate orig_inty i0 arin >=> 
         do { xa <- ftake vin_ty
            ; ya <- optzr (arout > 1) $ fneweref ("ya" ::: vout_ty)
            ; ftimes zERO m $ \cnt -> 
                 let offin  = embed (cnt .* i)
                     offout = embed (cnt .* j)
                     st = RwState { rws_in = DoRw xa offin
                                  , rws_out = doRw arout ya offout }
                 in fembed (act venv st)
            ; when (arout > 1) $ femit ya 
            }


{-------------------------------------------------------------------------
[DU3] X         -> b^j       ~~~> X^m           -> (b*j*m)
-------------------------------------------------------------------------}
vect_du3 :: DynFlags -> CTy -> LComp -> Int -> NMul -> VecM DelayedVectRes
vect_du3 dfs cty lcomp j (NMul m)
  = mkDVRes zirbody "DU3" loc vin_ty vout_ty
  where
    loc        = compLoc lcomp
    orig_outty = yldTyOfCTy cty
    vin_ty     = inTyOfCTy cty
    vout_ty    = mkVectTy orig_outty arout
    arout      = j*m -- size of output array

    act venv st = rwTakeEmitIO venv st lcomp

    zirbody :: VecEnv -> Zr ()
    zirbody venv = do
      ya <- optzr (arout > 1) $ fneweref ("ya" ::: vout_ty) 
      ftimes zERO m $ \cnt ->
        let offout = embed (cnt .* j)
            st = RwState { rws_in = DoNotRw, rws_out = doRw arout ya offout }
        in fembed (act venv st)
      when (arout > 1) $ femit ya

