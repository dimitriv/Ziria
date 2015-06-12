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
{-# LANGUAGE  QuasiQuotes, GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-binds -Werror #-}

module Ziria.Codegen.CgPerm ( genPermute ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.List (elemIndex, foldl')
import qualified Data.Loc
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Language.C.Quote.Base (ToConst(..))
import qualified Language.C.Pretty as P
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ 

import Ziria.BasicTypes.AstExpr
import Ziria.BasicTypes.AstComp
import Ziria.BasicTypes.PpExpr
import Ziria.BasicTypes.PpComp
import {-# SOURCE #-} Ziria.Codegen.CgExpr
import Ziria.Codegen.CgHeader
import Ziria.Codegen.CgLUT
import Ziria.Codegen.CgMonad
import Ziria.Codegen.CgRuntime
import Ziria.Codegen.CgTypes
import qualified Ziria.Utils.GenSym as GS

import Opts

newtype HexConst = HexConst Integer

instance ToConst HexConst where
    toConst (HexConst i) loc = C.LongIntConst ("0x"++showHex i "U") C.Unsigned i loc

-- The semantics of the bit permutation primitive, bperm:
--
-- After executing:
--
--   dest := bperm src perm
--
-- The following holds:
--
--   dest[i] = src[perm[i]]
--
genPermute :: DynFlags -> Exp Ty -> Exp Ty -> Cg C.Exp
genPermute dflags e1 e2 = do
    -- Allocate space for permuted array
    ctempid       <- genSym "permutation"
    let ctemp     =  [cexp|$id:ctempid|]
    
    appendDecl $ codeGenDeclGroup ctempid (info e1)

    -- Generate code for e1
    ce1 <- codeGenExp dflags e1
    case unExp e2 of
      EValArr vals | not (isDynFlagSet dflags NoLUT) -> fastPath ce1 vals ctemp
      _                                              -> slowPath ce1 ctemp
    return ctemp
  where
    TArr (Literal n) TBit = info e1

    fastPath :: C.Exp -> [Val] -> C.Exp -> Cg ()
    fastPath csrc perm0 cdest = do
        w <- calcLUTIdxWidth

        -- Generate the static LUT declaration
        clut <- genSym "permlut"
        appendTopDecl [cdecl|static typename int32 $id:clut[$int:(2^w)][$int:(n `quot` w)][$int:(getBitArrayByteLength n)] = $init:(genPermLUT w);|]

        genUpdateSSE clut w
      where
        perm :: [Int]
        perm = [i | VInt i <- perm0]

{-
        genUpdate :: String -> Int -> Cg ()
        genUpdate clut w = do
            preidx   <- genSym "pre_lut_idx"
            idx      <- genSym "lut_idx"
            off      <- genSym "off"
            bitsleft <- genSym "bitsleft"

            let init   = [[cstm|$cdest[$int:i]  = $id:clut[$id:idx][0][$int:i];|]       | i <- [0..getBitArrayByteLength n - 1]]
            let update = [[cstm|$cdest[$int:i] |= $id:clut[$id:idx][$id:off][$int:i];|] | i <- [0..getBitArrayByteLength n - 1]]
            let loop   = if n `quot` w == 1
                         then []
                         else [[cstm|for ($id:off = 1; $id:off < $int:(n `quot` w); ++$id:off)
                                     {
                                       $id:preidx >>= $int:w;
                                       $id:bitsleft -= $int:w;
                                       if ($id:bitsleft < $int:w) {
                                           $id:preidx = (((typename uint32) $csrc[($id:off*$int:w)/32])   >> (($id:off*$int:w)%32))
                                                      | (((typename uint32) $csrc[($id:off*$int:w)/32+1]) << (32-(($id:off*$int:w)%32)));
                                           $id:bitsleft = 32;
                                       }
                                       $id:idx = $id:preidx & $const:(HexConst (2^w-1));
                                       $stms:update
                                     }
                                |]]
        
            appendStmt [cstm|{typename uint32 $id:preidx;
                              typename uint32 $id:idx;
                              unsigned int    $id:off;
                              unsigned int    $id:bitsleft = 32;

                              $id:preidx = $csrc[0];
                              $id:idx    = $id:preidx & $const:(HexConst (2^w-1));
                              $stms:init

                              $stms:loop
                             }|]
-}

        genUpdateSSE :: String -> Int -> Cg ()
        genUpdateSSE clut w = do
            preidx   <- genSym "pre_lut_idx"
            idx      <- genSym "lut_idx"

            (temp_decls, temp_stms, temps) <- inNewBlock $ permTemps n

            appendStmt [cstm|{$decls:temp_decls
                              typename uint32 $id:preidx;
                              typename uint32 $id:idx;

                              $stms:temp_stms
                              $stms:(concat [loadLutidxAndUpdate preidx idx temps off | off <- [0..n `div` w - 1]])
                              $stms:(store temps n 0)
                             }|]
          where
            permTemps :: Int -> Cg [String]
            permTemps w | w <= 0 =
                return []

            permTemps w | w >= 128 = do
                temp <- genSym "perm128_"
                appendDecl [cdecl|typename __m128i $id:temp;|]
                temps <- permTemps (w - 128)
                return (temp : temps)

            permTemps w = do
                temp <- genSym "perm32_"
                appendDecl [cdecl|typename uint32 $id:temp;|]
                temps <- permTemps (w - 32)
                return (temp : temps)

            store :: [String] -> Int -> Int -> [C.Stm]
            store (temp:temps) w i | w >= 128 =
                  [cstm|_mm_storeu_si128((typename __m128i*) &($cdest[$int:i]), $id:temp);|]
                : store temps (w-128) (i+4)

            store (temp:temps) w i | w > 0 =
                  [cstm|$cdest[$int:i] = $id:temp;|]
                : store temps (w-32) (i+1)

            store _ _ _ =
                []

            loadLutidxAndUpdate :: String -> String -> [String] -> Int -> [C.Stm]
            loadLutidxAndUpdate preidx idx temps off = loadLutIdx off ++ updateDest off temps n 0
              where
                loadLutIdx :: Int -> [C.Stm]
                -- We can pull out the first subword easily.
                loadLutIdx 0 =
                    [[cstm|$id:preidx = $csrc[0];|]
                    ,[cstm|$id:idx    = $id:preidx & $const:(HexConst (2^w-1));|]
                    ]
                -- If this subword spans a 32-bit boundary, we need to load the LUT
                -- index from two successive words
                loadLutIdx off | (off*w `div` 32) /= ((off*w+w-1) `div` 32) =
                    [[cstm|$id:preidx = (((typename uint32) $csrc[$int:(off*w `div` 32)])   >> $int:((off*w) `mod` 32))
                                      | (((typename uint32) $csrc[$int:(off*w `div` 32+1)]) << $int:(32-(off*w) `mod` 32));|]
                    ,[cstm|$id:idx    = $id:preidx & $const:(HexConst (2^w-1));|]
                    ]
                -- Otherwise we can just shift preidx
                loadLutIdx off =
                    [[cstm|$id:preidx >>= $int:w;|]
                    ,[cstm|$id:idx      = $id:preidx & $const:(HexConst (2^w-1));|]
                    ]

                updateDest :: Int -> [String] -> Int -> Int -> [C.Stm]
                updateDest 0 (temp:temps) w i | w >= 128 =
                      [cstm|$id:temp = _mm_loadu_si128((typename __m128i*) &($id:clut[$id:idx][0][$int:i]));|]
                    : updateDest 0 temps (w-128) (i+4)

                updateDest 0 (temp:temps) w i | w > 0 =
                      [cstm|$id:temp = $id:clut[$id:idx][0][$int:i];|]
                    : updateDest 0 temps (w-32)  (i+1)

                updateDest 0 _ _ _ =
                    []

                updateDest off (temp:temps) w i | w >= 128 =
                      [cstm|$id:temp = _mm_or_si128($id:temp, _mm_loadu_si128(((typename __m128i*) &($id:clut[$id:idx][$int:off][$int:i]))));|]
                    : updateDest off temps (w-128) (i+4)

                updateDest off (temp:temps) w i | w > 0 =
                      [cstm|$id:temp |= $id:clut[$id:idx][$int:off][$int:i];|]
                    : updateDest off temps (w-32)  (i+1)

                updateDest off _ _ _ =
                    []

        -- Generate the LUT initializer for the permutation given the width in
        -- bits of the LUT index.
        genPermLUT :: Int -> C.Initializer
        genPermLUT w = [cinit| { $inits:inits }|]
          where
            inits :: [C.Initializer]
            inits = [genPermLUTForIdx w v | v <- [0..2^w-1]]

        -- Generate the portion of the LUT for the permutation given the width
        -- in bits of the LUT index and the LUT index.
        genPermLUTForIdx :: Int -> Int -> C.Initializer
        genPermLUTForIdx w idx = [cinit| { $inits:inits }|]
          where
            inits :: [C.Initializer]
            inits = [genBitArray w idx off | off <- [0..n `quot` w - 1]]

        -- Given a starting offset in number of @w@-bit-groups, the width of the
        -- LUT index, and the index's value, generate an initializer for the bit
        -- array that is the value at this LUT index.
        genBitArray :: Int -> Int -> Int -> C.Initializer
        genBitArray w idx off = [cinit|{ $inits:inits }|]
          where
            inits :: [C.Initializer]
            inits = [word k | k <- [0..getBitArrayByteLength n - 1]]

            -- Bits that are set in the permutation when the @w@ bits starting
            -- at offset @off@ in the source have the value @v@.
            bitsSet :: [Int]
            bitsSet = [fromJust (elemIndex (off*w+i) perm) | i <- [0..w-1], testBit idx i]

            -- The value whose set bits are @bitsSet@.
            x :: Integer
            x = foldl' (.|.) 0 [1 `shiftL` fromIntegral k | k <- bitsSet ]

            -- The @k@th 32-bit word
            word :: Int -> C.Initializer
            word k = [cinit|$const:(HexConst ((x `shiftR` (k*32)) .&. 0xFFFFFFFF))|]

        -- Width of LUT index. It must evenly divide the bit-width of the array,
        -- and it must not cause us to generate LUT tables that are too big. We
        -- pick the largest width we can.
        calcLUTIdxWidth :: Cg Int
        calcLUTIdxWidth =
            case candidates of
              [] -> fail $ "Cannot LUT permutation of size " ++ show n
              _  -> return $ fromIntegral (last candidates)
          where
            n' :: Integer
            n' = fromIntegral n
            
            candidates :: [Integer]
            candidates = [i | i <- [1..n']
                            , n' `rem` i == 0
                            , let lutSizeInBytes = 2^i*(fromIntegral n `quot` i)*((fromIntegral n+7) `quot` 8)
                            , lutSizeInBytes < maxLUTSize dflags]

    slowPath :: C.Exp -> C.Exp -> Cg ()
    slowPath csrc cdest = do
        i       <- genSym "i"
        permbit <- genSym "permbit"
        cperm   <- codeGenExp dflags e2
        

        appendStmt [cstm|for(unsigned int $id:i = 0; $id:i < $int:(getBitArrayByteLength n); ++$id:i)
                         {
                           $cdest[$id:i] = 0;
                         }
                        |]
        let e_i  = [cexp|$id:i|]
        let e_permbit = [cexp|$id:permbit|]
        
        appendStmt [cstm|for(unsigned int $id:i = 0, $id:permbit; $id:i < $int:n; ++$id:i)
                         {
                           $id:permbit = $cperm[$id:i];
                           if ($(testArrBit csrc e_permbit))
                               $stm:(setArrBit cdest e_i)
                         }
                        |]

    testArrBit :: C.Exp -> C.Exp -> C.Exp
    testArrBit arr i = [cexp|($arr[$i/32] & (1 << ($i%32))) != 0|]

    setArrBit :: C.Exp -> C.Exp -> C.Stm
    setArrBit arr i = [cstm|$arr[$i/32] |= (1 << ($i%32));|]

