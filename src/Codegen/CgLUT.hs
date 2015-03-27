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

module CgLUT
  ( codeGenLUTExp
  , codeGenLUTExp_Mock
  ) where

import Opts
import AstExpr
import AstUnlabelled
import CtExpr
import Text.Parsec.Pos
import {-# SOURCE #-} CgExpr
import CgMonad hiding (State)
import CgTypes
import NameEnv
import Analysis.RangeAnal
import Analysis.DataFlow
import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( isJust, fromJust )
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.HughesPJ 
import LUTAnalysis
import qualified Data.Hashable as H
import PpExpr ()
import Outputable 
import Data.Bits
import Data.Word

{-------------------------------------------------------------------------------
   Infrastructure 
--------------------------------------------------------------------------------}

lutIndexTypeByWidth :: Monad m => Int -> m C.Type
-- ^ lutIndexTypeByWidth
-- Input: input width
-- Returns: type to use for the LUT index
-- Note:
-- The default MAX LUT size at the moment (Opts.mAX_LUT_SIZE_DEFAULT)
-- is 256*1024 bytes which is 2^18 entries, so right just above 16
-- bits, when we emit 1 byte worth of output. So we have to be able to
-- support indexes higher than 16 bits.
lutIndexTypeByWidth n
    | n <= 8    = return [cty|typename uint8|]
    | n <= 16   = return [cty|typename uint16|]
    | n <= 32   = return [cty|typename uint32|]
    | otherwise
    = fail "lutIndexTypeByWidth: need at most 32 bits for the LUT index"


-- | Shift ce by n bits left.
cExpShL :: C.Exp -> Int -> C.Exp
cExpShL ce 0 = ce
cExpShL ce n = [cexp| $ce << $int:n |]

-- | Shift ce by n bits right.
cExpShR :: C.Exp -> Int -> C.Exp
cExpShR ce 0 = ce
cExpShR ce n = [cexp| $ce >> $int:n |]

-- | Keep the low n bits of ce only when ce fits in a register.
cMask :: C.Exp -> Int -> C.Exp
cMask ce n = [cexp| $ce & $int:mask|]
  where mask = 2^(fromIntegral w) - 1

-- | Cast this variable to bit array pointer.
varToBitArrPtr :: EId -> Cg C.Exp
varToBitArrPtr v = do
  varexp <- lookupVarEnv v
  -- TODO: by-ref variables?
  if isArrayTy (nameTyp v)
      then [cexp| (typename BitArrPtr) $varexp    |]
      else [cexp| (typename BitArrPtr) (& $varexp)|]

-- Like csrc/bitArrRead.h for byte-aligned bit-arrays.
cgBitArrRead_alg :: C.Exp -> Int -> Int -> C.Exp -> Cg ()
-- ^ Pre: pos and len are multiples of 8; src, tgt are of type BitArrPtr
cgBitArrRead_alg src pos len tgt
  -- Register lengths
  | len == 8 
  = appendStmt [cstm| *$tgt = $src[$int:(pos `div` 8)];|]
  | len == 16
  = appendStmt [cstm| *((uint16 *) $tgt) = *(uint16 *) (& $src[sidx]);|]
  | len == 32
  = appendStmt [cstm| *((uint32 *) $tgt) = *(uint32 *) (& $src[sidx]);|]
  | len == 64
  = appendStmt [cstm| *((uint64 *) $tgt) = *(uint64 *) (& $src[sidx]);|]
  | otherwise
  = appendStmt [cstm| blink_copy((void *) $tgt, len `div` 8, (void *) (& $src[sidx]);|]
  where sidx = pos `div` 8

{-------------------------------------------------------------------------------
   Packing index variables (see also Note [LUT Packing Strategy])
--------------------------------------------------------------------------------}
packIdx :: VarUsePkg
        -> [EId]   -- ^ Variables to pack
        -> C.Exp   -- ^ A C expression for the index variable
        -> C.Type  -- ^ The actual index type (lutIndeTypeByWidth)
        -> Cg ()
packIdx pkg vs idx idx_ty = go vs 0
  where go [] w     = return ()
        go (v:vs) w = pack_idx_var pkg v idx idx_ty w >>= go vs

pack_idx_var :: VarUsePkg -- ^ Variable use package
             -> EId       -- ^ Variable to pack
             -> C.Exp     -- ^ Index
             -> C.Type    -- ^ Index type (lutIndexTypeByWidth)
             -> Int       -- ^ Offset to place the variable at
             -> Cg Int    -- ^ Final offset (for the next variable)
pack_idx_var pkg v idx idx_ty pos
  | TArray _ base_ty <- v_ty
  , Just (lidx,hidx) <- inArrSlice pkg idx
  = do base_w <- tyBitWidth base_ty
       let slice_w = (hidx-lidx+1)*base_w -- ^ slice width
       varexp <- lookupVarEnv v
       let tmp_var = [cexp|(($ty:idx_ty) (* $varexp))|]
           slice   = tmp_var `cExpShR` (lidx*base_w) `cMask` slice_w
           rhs     = slice `cExpShL` pos
       appendStmt $ [cstm| $idx |= $rhs; |]
       return (pos+slice_w)
  | otherwise
  = do w <- inVarBitWidth pkg v
       varexp <- lookupVarEnv v
       let rhs = [cexp|(($ty:idx_ty) $varexp)|] `cMask` w `cExpShL` pos
       appendStmt $ [cstm| $idx |= $rhs; |]
       return (pos+w)

{-------------------------------------------------------------------------------
   Unpacking index variables (see also Note [LUT Packing Strategy])
--------------------------------------------------------------------------------}
unpackIdx :: VarUsePkg 
          -> [EId]     -- ^ Variables to unpack
          -> C.Exp     -- ^ A C expression for the index variable
          -> C.Type    -- ^ The actual index type (lutIndeTypeByWidth)
          -> Cg ()
-- ^ NB: Destructively updates the index variable!
unpackIdx pkg vs idx idx_ty = go vs
  where go [] w     = return ()
        go (v:vs) w = unpack_idx_var pkg v idx idx_ty w >> go vs

unpack_idx_var :: VarUsePkg -- ^ Variable use package
               -> EId       -- ^ Variable to pack
               -> C.Exp     -- ^ Index
               -> Cg ()     -- ^ Final offset (for the next variable)
-- ^ NB: Unpacking index variables is not performance critical since
-- it is only used during LUT generation but not during execution. So
-- there aren't any clever fast paths here.
unpack_idx_var pkg v idx idx_ty
  | TArray _ base_ty <- v_ty
  , Just (lidx,hidx) <- inArrSLice pkg idx
  = do base_w <- tyBitWidth base_ty
       vptr <- varToBitArrPtr v
       let slice_w = base_w*(hidx-lidx+1)
       appendStmt [cstm|bitArrWrite($idx_ptr,$int:(lidx*base_w),$slice_w,$vptr);|]
       let new_idx = idx `cExpShR` slice_w
       appendStmt $ [cstm| $idx = $new_idx;|]
  | otherwise
  = do w    <- inVarBitWidth pkg v
       vptr <- varToBitArrPtr v
       appendStmt [cstm|bitArrWrite((BitArrPtr) & $idx,0,$int:w,$varexp_ptr);|]
       let new_idx = idx `cExpShR` w
       appendStmt $ [cstm| $idx = $new_idx;|]
  where idx_ptr      = [cexp| (BitArrPtr) & $idx |]

{-------------------------------------------------------------------------------
   Packing (byte-aligned) output variables (see also Note [LUT Packing Strategy])
--------------------------------------------------------------------------------}

packOutVars :: VarUsePkg          -- ^ Usage info
            -> [(EId, Maybe EId)] -- ^ Output vars and assign-masks
            -> C.Exp              -- ^ Of BitArrPtr type
            -> Cg ()
-- ^ NB: This is not perf critical as it is only used for LUT generation.
packOutVars pkg vs tgt = go vs 0
  where go [] _       = return ()
        go (v:vs) pos = pack_outvar pkg v tgt pos >>= go vs

pack_out_var :: VarUsePkg
             -> (EId, Maybe EId)
             -> C.Exp
             -> Int
             -> Cg Int
pack_out_var pkg (v,v_asng_mask) tgt pos = do
  vptr <- varToBitArrPtr v
  total_width <- outVarBitWidth pkg v
  -- ^ NB: w includes width for v_asgn_mask already!
  let w = if isJust v_asgn_mask then total_width / 2 else total_width
  appendStmt [cstm|bitArrWrite(vptr,$int:pos,$int:w,$tgt); |]
  -- | Write the mask if there is one
  case v_asgn_mask of
    Just v' -> do mptr <- varToBitArrPtr v'
                  appendStmt [cstm| bitArrWrite(mptr,$int:(pos+w),$int:w,$tgt); |]
    Nothing -> return ()
  -- | Return new position
  return (pos+total_width)

{-------------------------------------------------------------------------------
   Unpacking (byte-aligned) output vars (see also Note [LUT Packing Strategy])
--------------------------------------------------------------------------------}

unpackOutVars :: VarUsePkg          -- ^ Usage info
              -> [(EId, Maybe EId)] -- ^ Output vars and assign-masks
              -> C.Exp              -- ^ Already of BitArrPtr type
              -> Cg ()
unpackOutVars pkg vs src = go vs 0
  where go [] _       = return ()
        go (v:vs) pos = unpack_out_var pkg v src pos >>= go vs

unpack_out_var :: VarUsePkg
               -> (EId, Maybe EId)
               -> C.Exp
               -> Int
               -> Cg Int
unpack_out_var pkg (v, Nothing) src pos = do
  vptr <- varToBitArrPtr v
  w    <- outVarBitWidth pkg v -- ^ Post: w is multiple of 8
  cgBitArrRead_alg src pos w vptr
  return (pos+w)

unpack_out_var pkg (v, Just {}) src pos = do
  vptr    <- varToBitArrPtr v
  total_w <- outVarBitWidth pkg v
  let w  = total_w / 2
  let mask_ptr = [cexp| & src[$int:((pos+w) `div` 8)]|]
  cgBitArrLUTMask vptr mask_ptr src w
  return (pos + total_w)

cgBreakDown :: Int -> C.Exp -> [C.Exp]
-- ^ Breakdown to 64,32,16,8 multiples ... (later on we should als add SSE 128/256)
cgBreakDown 8  ptr = [ptr]
cgBreakDown 16 ptr = [[|(uint16 *) ptr|]]
cgBreakDown 32 ptr = [[|(uint32 *) ptr|]]
cgBreakDown 64 ptr = [[|(uint64 *) ptr|]]
cgBreakDown n  ptr 
  = if n - 64 > 0 then [|(uint64 *) ptr|] : cgBreakDown (n-64) [cexp| ptr + 8 |] else 
    if n - 32 > 0 then [|(uint32 *) ptr|] : cgBreakDown (n-32) [cexp| ptr + 4 |] else 
    if n - 16 > 0 then [|(uint16 *) ptr|] : cgBreakDown (n-16) [cexp| ptr + 2 |] else 
    if m - 8  > 0 then [ptr] else []

cgBitArrLUTMask :: C.Exp -- ^ output variable BitArrPtr
                -> C.Exp -- ^ mask BitArrPtr  (1 means 'set')
                -> C.Exp -- ^ LUT entry BitArrPtr
                -> Int   -- ^ BitWidth
                -> Cg ()
-- ^ Implements: vptr = (lptr & mptr) | (vptr & ~ mptr)
cgBitArrLUTMask vptr mptr lptr width
  = let vptrs = cgBreakDown width vptr 
        mptrs = cgBreakDown width mptr
        lptrs = cgBreakDown width lptr
        mk_stmt (v,m,l) = [cstm| *v = (*l & *m) | (*v & ~ *m);|]
    in sequence_ (appendStmt . mk_stmt) vptrs mptrs lptrs

