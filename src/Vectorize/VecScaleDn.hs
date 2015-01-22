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

module VecScaleDn ( doVectCompDD ) where

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

import Outputable
import Text.PrettyPrint.HughesPJ

doVectCompDD :: DynFlags -> CTy -> LComp -> SFDD -> VecM DelayedVectRes
doVectCompDD dfs cty lcomp (SFDD1 (DivsOf i i0 i1))
  = vect_dd1 dfs cty lcomp i i0 i1
doVectCompDD dfs cty lcomp (SFDD2 (DivsOf j j0 j1))
  = vect_dd2 dfs cty lcomp j j0 j1
doVectCompDD dfs cty lcomp (SFDD3 (DivsOf i i0 i1) (DivsOf j j0 j1))
  = vect_dd3 dfs cty lcomp i i0 i1 j j0 j1

{-------------------------------------------------------------------------------
  [DD1] a^(i0*i1)   -> X ~~~> (a*i0)^i1 -> X
-------------------------------------------------------------------------------}
vect_dd1 :: DynFlags
         -> CTy -> LComp -> Int -> NDiv -> NDiv -> VecM DelayedVectRes
vect_dd1 dfs cty lcomp i (NDiv i0) (NDiv i1)
  = mkDVRes zirbody "DD1" loc vin_ty vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = yldTyOfCTy cty
    arin       = i0

    vin_ty_big = mkVectTy orig_inty (i0*i1)

    -- NB: action does not rewrite emit(s)
    action venv iidx tidx xa
      = rwTakeEmitIO venv (rw_take arin iidx tidx xa)
                          (rw_takes iidx tidx xa) mEmit mEmits lcomp

    zirbody :: VecEnv -> Zr EId
    zirbody venv
      = fleteref ("xa" ::: vin_ty_big) $ \xa ->
        fleteref ("iidx" ::: tint ::= zERO) $ \iidx ->
        fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
        do { ftimes zERO i1 $ \cnt ->
               do x <- ftake vin_ty
                  if arin == 1
                    then xa .! cnt   .:= x
                    else xa .! ((cnt .* arin) :+ arin) .:= x
           ; fembed (action venv iidx tidx xa) }

{-------------------------------------------------------------------------------
  [DD2] X -> b^(j0*j1)   ~~~> X -> (b*j0)^j1
-------------------------------------------------------------------------------}
vect_dd2 :: DynFlags
         -> CTy -> LComp -> Int -> NDiv -> NDiv -> VecM DelayedVectRes
vect_dd2 dfs cty lcomp j (NDiv j0) (NDiv j1)
  = mkDVRes zirbody "DD2" loc orig_inty vout_ty
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vout_ty    = mkVectTy orig_outty arout
    arout      = j0

    vout_ty_big = mkVectTy orig_outty (j0*j1)

    -- NB: action does not rewrite emit(s)
    action venv oidx tidx ya
      = rwTakeEmitIO venv mTake1 mTake
              (rw_emit arout oidx tidx ya)
              (rw_emits oidx tidx ya) lcomp

    zirbody :: VecEnv -> Zr EId
    zirbody venv
      = fleteref ("ya" ::: vout_ty_big) $ \ya ->
        fleteref ("oidx" ::: tint ::= zERO) $ \oidx ->
        fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
        do { v <- fembed (action venv oidx tidx ya)
           ; ftimes zERO j1 $ \cnt ->
               if arout == 1
                 then femit (ya .! cnt)
                 else femit (ya .!((cnt .*arout) :+ arout))
           ; freturn _fI v
           }

{-------------------------------------------------------------------------------
  [DD3] a^(i0*i1) -> b^(j0*j1) ~~~>    (a*i0)^i1 -> (b*j0)^j1
-------------------------------------------------------------------------------}
vect_dd3 :: DynFlags -> CTy -> LComp
         -> Int -> NDiv -> NDiv
         -> Int -> NDiv -> NDiv -> VecM DelayedVectRes
vect_dd3 dfs cty lcomp i (NDiv i0) (NDiv i1)
                       j (NDiv j0) (NDiv j1) = do
  liftIO $ verbose dfs $ vcat [ text "vect_dd3, lcomp:"
                              , ppr lcomp
                              , ppr (compInfo lcomp)
                              , hsep [ text "i  =" <+> ppr i
                                     , text "i0 =" <+> ppr i0
                                     , text "i1 =" <+> ppr i1 ]
                              , hsep [ text "j  =" <+> ppr j
                                     , text "j0 =" <+> ppr j0
                                     , text "j1 =" <+> ppr j1 ]
                              ]
  r <- mkDVRes zirbody "DD3" loc vin_ty vout_ty
  liftIO $ do { c <- dvr_comp r
              ; verbose dfs $ vcat [ text "vect_dd3, result:", ppr c ]
              }
  return r
  where
    loc        = compLoc lcomp
    orig_inty  = inTyOfCTy cty
    orig_outty = yldTyOfCTy cty
    vin_ty     = mkVectTy orig_inty  arin
    vout_ty    = mkVectTy orig_outty arout
    arin       = i0
    arout      = j0

    vin_ty_big  = mkVectTy orig_inty (i0*i1)
    vout_ty_big = mkVectTy orig_outty (j0*j1)

    -- NB: action does not rewrite emit(s)
    action venv iidx oidx tidx xa ya
      = rwTakeEmitIO venv (rw_take arin iidx tidx xa)
                          (rw_takes iidx tidx xa)
                          (rw_emit arout oidx tidx ya)
                          (rw_emits oidx tidx ya) lcomp

    zirbody :: VecEnv -> Zr EId
    zirbody venv
      = fleteref ("xa" ::: vin_ty_big)  $ \xa ->
        fleteref ("ya" ::: vout_ty_big) $ \ya ->
        fleteref ("iidx" ::: tint ::= zERO) $ \iidx ->
        fleteref ("oidx" ::: tint ::= zERO) $ \oidx ->
        fleteref ("tidx" ::: tint ::= zERO) $ \tidx ->
        do { ftimes zERO i1 $ \cnt ->
               do { x <- ftake vin_ty
                  ; if arin == 1
                    then xa .! cnt   .:= x
                    else xa .! ((cnt .* arin) :+ arin) .:= x
                  }
           ; v <- fembed (action venv iidx oidx tidx xa ya)
           ; ftimes zERO j1 $ \cnt ->
               if arout == 1 
                 then femit (ya .! cnt) 
                 else femit (ya .!((cnt .*arout) :+ arout))
           ; freturn _fI v
           }
