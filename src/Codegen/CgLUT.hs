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
  , pprLUTStats
  , shouldLUT
  ) where


import Opts
import AstExpr
import AstUnlabelled

import Text.Parsec.Pos

import {-# SOURCE #-} CgExpr
import CgMonad hiding (State)
import CgTypes
-- import SysTools
import Analysis.Range
import Analysis.UseDef

import Control.Monad.IO.Class (liftIO)
import Data.Bits
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe ( isJust, fromJust )
import Data.Word
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.HughesPJ 

import LUTAnalysis

import qualified Data.Hashable as H

import CtExpr

import PpExpr ()

import Outputable 

--  import System.Exit ( exitFailure )

-- XXX TODO
--
-- These are bugs!
--
-- * When generating a LUT, we should only iterate over the values that are in a
-- variable's range, i.e., those that we know are used.
--
-- * We must know the range of any variable that is used as part of an array
-- index calculation in an expression if we want LUT the expression. Otherwise
-- we may access an array at an illegal index.

-- If we need more than 32 bits for the index, the LUT is going to be at least
-- 2^32 bytes, so
lutIndexTypeByWidth :: Monad m => Int -> m C.Type
lutIndexTypeByWidth n
    | n <= 8    = return [cty|typename uint8|]
    | n <= 16   = return [cty|typename uint16|]
    | n <= 32   = return [cty|typename uint32|]
    | otherwise
    = fail "lutIndexTypeByWidth: need at most 32 bits for the LUT index"


-- We know that the index is at most 4 bytes and we can pack using
-- simple shifts, without involving the more expensive
-- bitArrRead/bitArrWrite library functions.

packIdx :: Map EId Range   -- Ranges
                  -> [EId]   -- Variables
                  -> C.Exp        -- A C expression for the index variable
                  -> C.Type       -- The actual index type (typically unsigned int)
                  -> Cg ()
packIdx ranges vs tgt tgt_ty = go vs 0
  where go [] _ = return ()
        go (v:vs) pos
         | let ty = nameTyp v
         , isArrayTy ty
         = do { w <- varBitWidth ranges v
              ; let mask = 2^w - 1

              ; varexp <- lookupVarEnv v

              -- fast path for <= 1 byte variables
              ; let (TArray len bty) = ty
              ; rhs <- 
                 case (bty, len) of
                   (TBit, Literal n) | n <= 8 -> 
                     return $ [cexp|((* $varexp) & $int:mask)|] `cExpShL` pos 
                   _otherwise -> 
                     do { z <- genSym "__z"
                        ; appendDecl $ [cdecl| $ty:tgt_ty * $id:z;|]
                        ; appendStmt $ [cstm| $id:z = ($ty:tgt_ty *) $varexp; |]
                        ; return $ [cexp|((* $id:z) & $int:mask)|] `cExpShL` pos
                        }
              ; appendStmt $ [cstm| $tgt |= $rhs; |]
              ; go vs (pos+w) }
         | otherwise
         = do { w <- varBitWidth ranges v
              ; let mask = 2^w - 1

              ; varexp <- lookupVarEnv v
              -- appendStmt $ [cstm| $tgt |= ((($ty:tgt_ty) $varexp) & $int:mask) << $int:pos; |]
              ; let rhs = [cexp|(($ty:tgt_ty) $varexp) & $int:mask|] `cExpShL` pos
              ; appendStmt $ [cstm| $tgt |= $rhs; |]

              ; go vs (pos+w) }

cExpShL :: C.Exp -> Int -> C.Exp
cExpShL ce 0 = ce
cExpShL ce n = [cexp| $ce << $int:n |]

cExpShR :: C.Exp -> Int -> C.Exp
cExpShR ce 0 = ce
cExpShR ce n = [cexp| $ce >> $int:n |]


unpackIdx :: [EId]   -- Variables
          -> C.Exp   -- A C expression representing the index
          -> C.Type  -- The index type (typically unsigned int)
          -> Cg ()
unpackIdx xs src src_ty = go xs 0
  where go [] _ = return ()
        go (v:vs) pos
         | let ty = nameTyp v
         , TArray basety alen <- ty
         , let bytesizeof = tySizeOf_C ty
         = do { varexp <- lookupVarEnv v
              ; w <- tyBitWidth ty

              ; let mask = 2^w - 1

              ; tmpname_aux <- freshVar "tmp_aux"
              ; tmpname <- freshVar "tmp"

              ; appendDecl [cdecl| $ty:src_ty $id:tmpname_aux = $(src `cExpShR` pos); |]

              ; appendDecl [cdecl| $ty:src_ty $id:tmpname = $id:tmpname_aux & $int:mask; |]
              ; appendStmt [cstm| memcpy((void *) $varexp, (void *) & ($id:tmpname), $bytesizeof);|]

              ; go vs (pos+w) }
         | otherwise
         = do { varexp <- lookupVarEnv v
              ; w <- tyBitWidth (nameTyp v)
              ; let mask = 2^w - 1
              ; appendStmt $ [cstm| $varexp = (($src >> $int:pos)) & $int:mask; |]
              ; go vs (pos+w) }



packByteAligned :: Map EId Range -- Ranges
               -> [EId]            -- Variables
               -> C.Exp                 -- A C expression of type BitArrPtr
               -> Cg ()
packByteAligned ranges vs tgt = go vs 0
  where go [] _ = return ()
        go (v:vs) pos
         | isArrayTy (nameTyp v)
         = do { w <- varBitWidth ranges v
              ; varexp <- lookupVarEnv v
              ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (byte_align (pos+w)) } -- Align for next write!
         | otherwise
         = do { w <- varBitWidth ranges v
              ; varexp <- lookupVarEnv v
              ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) & $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (byte_align (pos+w)) }  -- Align for next write!

byte_align n = ((n+7) `div` 8) * 8

unpackByteAligned :: [EId] -- Variables
                   -> C.Exp   -- A C expression of type BitArrPtr
                   -> Cg Int  -- Last byte position
unpackByteAligned xs src = go xs 0
  where go [] pos = return (pos `div` 8)
        go (v:vs) pos
         | let ty = nameTyp v
         , Just base <- isArrayTy_maybe ty
         = do { varexp <- lookupVarEnv v
              ; w <- tyBitWidth ty
              ; let bytes_to_copy = byte_align w `div` 8
              ; case base of
                  TBit -- a Bit array
                    | bytes_to_copy == 1 
                    -> appendStmt $ [cstm| *$varexp = $src[$int:(pos `div` 8)]; |]
                  _otherwise
                    -> appendStmt $ [cstm| memcpy((void *) $varexp, (void *) & $src[$int:(pos `div` 8)], $int:bytes_to_copy);|]
              ; go vs (byte_align (pos+w)) } -- Align for next read!
         | TBit <- nameTyp v
         = do { varexp <- lookupVarEnv v
              ; appendStmt $ [cstm| $varexp = $src[$int:(pos `div` 8)]; |]
              -- ; w <- tyBitWidth ty
              -- ; appendStmt $ [cstm| blink_copy((void *) & $varexp, (void *) & $src[$int:(pos `div` 8)], $int:(byte_align w `div` 8));|]
              ; go vs (byte_align (pos+8)) } -- Align for next read!

         | otherwise
         = do { let ty = nameTyp v
              ; varexp <- lookupVarEnv v
                -- NEW: 
              ; assignByVal ty ty varexp [cexp| *(($ty:(codeGenTy ty) *) & $src[$int:(pos `div` 8)])|] 
              ; w <- tyBitWidth ty               
              -- OLD ; appendStmt $ [cstm| blink_copy((void *) & $varexp, (void *) & $src[$int:(pos `div` 8)], $int:(byte_align w `div` 8));|]
              ; go vs (byte_align (pos+w)) } -- Align for next read!


codeGenLUTExp_Mock :: DynFlags -> Maybe SourcePos
                   -> Map EId Range -> Exp -> Cg C.Exp
codeGenLUTExp_Mock dflags loc r  e
  = do { (inVars, outVars, allVars) <- inOutVars r e
       ; _ <- codeGenExp dflags $ ePrint Nothing True $
                                  eVal Nothing TString (VString (show loc))
       ; _ <- codeGenExp dflags $ ePrint Nothing True $
                                  eVal Nothing TString (VString "INVARS")
       ; cg_print_vars dflags inVars
       ; _ <- codeGenExp dflags $ ePrint Nothing True $
                                  eVal Nothing TString (VString "OUTVARS")
       ; r <- codeGenExp dflags e
       ; cg_print_vars dflags outVars
       ; return r
       }


codeGenLUTExp :: DynFlags
              -> Map EId Range
              -> Exp
              -> Maybe EId -- If set then use this specific name for
                                  -- storing the final result
              -> Cg C.Exp
codeGenLUTExp dflags ranges e mb_resname
  = case shouldLUT dflags ranges e of
      Left err ->
        do { verbose dflags $ vcat [ text "Asked to LUT un-LUTtable expression:" <+> text err
                                   , nest 4 (ppr e) ]
           ; codeGenExp dflags e }
      Right True  -> lutIt mb_resname
      Right False ->
        do { verbose dflags $
             vcat [ text "Asked to LUT an expression we wouldn't normally LUT:"
                  , nest 4 (ppr e) 
                  ] 
           ; lutStats <- calcLUTStats ranges e
           ; if lutTableSize lutStats >= aBSOLUTE_MAX_LUT_SIZE
             then do { verbose dflags $
                       vcat [ text "LUT way too big!"
                            , fromJust (pprLUTStats dflags ranges e) ]
                     ; codeGenExp dflags e }
             else lutIt mb_resname }
  where
    -- TODO: Document why this not the standard size
    -- but something much bigger? (Reason: alignment)
    aBSOLUTE_MAX_LUT_SIZE :: Integer
    aBSOLUTE_MAX_LUT_SIZE = 1024*1024

    lutIt :: Maybe EId -> Cg C.Exp
    lutIt mb_resname = do
        (inVars, outVars, allVars) <- inOutVars ranges e

        verbose dflags $
          vcat [ text "Creating LUT for expression:" 
               , nest 4 (ppr e)
               , nest 4 $ vcat [ text "Variable ranges:"
                               , pprRanges ranges ]
               , fromJust (pprLUTStats dflags ranges e) ]

        let resultInOutVars
             | Just v <- expResultVar e
             , v `elem` outVars = Just v
             | otherwise = Nothing

        let h = H.hash (show e)
        -- liftIO $ putStrLn $ "Hash = " ++ show (H.hash (show e))
        hs <- getLUTHashes
        let gen_lut_action
              = genLUT dflags ranges inVars (outVars,resultInOutVars) allVars e
        clut <-
          if isDynFlagSet dflags NoLUTHashing
          then do { lgi <- gen_lut_action
                  ; setLUTHashes $ (h,lgi):hs
                  ; return $ lgi_lut_var lgi
                  }
          else
             case lookup h hs of
                Just clut
                  -> do { liftIO $
                          putStrLn $ "Expression to LUT is already lutted!"
                        ; return $ lgi_lut_var clut
                        }
                Nothing
                  -> do { liftIO $ putStrLn $ "Invoking genLUT"
                        ; clut_gen_info <- gen_lut_action
                        ; setLUTHashes $ (h,clut_gen_info):hs
                        ; return $ lgi_lut_var clut_gen_info
                        }

        genLUTLookup dflags ranges
                        inVars (outVars,resultInOutVars)
                        clut (ctExp e) mb_resname e

genLUT :: DynFlags -- ^ Flags
       -> Map EId Range
       -> [EId]              -- ^ Input variables
       -> ([EId], Maybe EId) -- ^ Output variables + result if not in outvars
       -> [EId]  -- ^ All used variables
       -> Exp   -- ^ Expression to LUT
       -> Cg LUTGenInfo
genLUT dflags ranges inVars (outVars, res_in_outvars) allVars e = do
    inBitWidth <- varsBitWidth ranges inVars
    cinBitType <- lutIndexTypeByWidth inBitWidth

    -- 'clut' is the C variable that will hold the LUT.
    clut <- freshVar "clut"
    -- 'clutentry' is the C variable to hold one entry in the lut
    clutentry <- freshVar "clutentry"

    -- 'cidx' is the C variable that we use to index into the LUT.
    cidx <- freshVar "idx"

    -- 'clutgen' is the generator function for this LUT
    clutgen <- freshVar "clut_gen"

    (defs, (decls, stms, outBitWidth))
        <- collectDefinitions $
           inNewBlock $
           -- Just use identity code generation environment
           extendVarEnv [(v,[cexp|$id:(name v)|]) | v <- allVars] $
           -- Generate local declarations for all input and output variables
           do { genLocalVarInits dflags allVars
              ; unpackIdx inVars [cexp|$id:cidx |] [cty|unsigned int|]

                -- DEBUG ONLY
              ; let dbg_idx = "___dbg_idx"
              ; appendDecl [cdecl| unsigned int $id:dbg_idx; |]

              ; appendStmt [cstm| $id:dbg_idx = 0; |]
              ; packIdx ranges inVars [cexp|$id:dbg_idx|] [cty|unsigned int|]
              ; appendStmt [cstm|if ($id:cidx != $id:dbg_idx) {
                                   printf("FATAL BUG in LUT generation:packIdx/unpackIdx mismatch!(%s)", $string:cidx);
                                   exit(-1);
                                 }|]
                -- END DEBUG

              ; mapM_ (\v -> ensureInRange v) inVars
              ; ce <- codeGenExp dflags e
              ; (outBitWidth, outVarsWithRes, result_env) <-
                  if (isJust res_in_outvars || ctExp e == TUnit) then
                    do { ow <- varsBitWidth_ByteAlign outVars
                         -- clutentry declaration and initialization to zeros
                         -- clutentry declaration and initialization to zeros
                       ; let entry_ty = TArray (Literal ow) TBit
                       ; g <- codeGenArrVal clutentry entry_ty 
                                                [eVal Nothing TBit (VBit False)]
                       ; appendDecl g
                       ; return (ow,outVars, []) }
                  else
                    do { let retty = ctExp e
                       ; resval <- freshName "res" retty Imm -- TODO: is this correct?
                       ; codeGenDeclGroup (name resval) retty >>= appendDecl
                       ; let outVarsWithRes = outVars ++ [resval]
                       ; ow <- varsBitWidth_ByteAlign outVarsWithRes
                         -- clutentry declaration and initialization to zeros
                       ; let entry_ty = TArray (Literal ow) TBit
                       ; g <- codeGenArrVal clutentry entry_ty 
                                                [eVal Nothing TBit (VBit False)]
                       ; appendDecl g
                       ; assignByVal retty retty
                                [cexp|$id:(name resval)|] ce
                       ; reswidth <- tyBitWidth retty
                       ; let renv =
                              [(resval, [cexp|$id:(name resval)|])]
                       ; return (ow, outVarsWithRes, renv) }
              ; extendVarEnv result_env $
                packByteAligned ranges outVarsWithRes [cexp| $id:clutentry |]

              ; return outBitWidth
              }

    -- make a 2-byte aligned entry.
    let lutEntryByteLen = ((((outBitWidth + 7) `div` 8) + 1) `div` 2) * 2

    let idxLen = (1::Word) `shiftL` inBitWidth
        lutbasety = namedCType $ "calign unsigned char"
        clutDecl = [cdecl| $ty:lutbasety
                                 $id:clut[$int:idxLen][$int:lutEntryByteLen];|]

    let clutgen_def
          =
            [cedecl|void $id:clutgen()
                     {
                        for(unsigned int $id:cidx = 0;
                                 $id:cidx < $int:idxLen;
                                 ($id:cidx)++)
                        {

                          $decls:decls
                          unsigned int b;
                          $stms:stms

                          for (b = 0; b < $int:(lutEntryByteLen); b++) {
                                $id:clut[$id:cidx][b] = $id:clutentry[b];
                          }
                        }
                     }
            |]

    appendTopDecl clutDecl

    appendTopDefs defs
    appendTopDef clutgen_def


    let res = LUTGenInfo { lgi_lut_var = [cexp|$id:clut|]
                         , lgi_lut_gen = [cstm|$id:clutgen();|]
                         }

    return res

  where
    -- Ensure that the given variable is in range
    ensureInRange :: EId -> Cg ()
    ensureInRange v
     | Just (Range l h) <- Map.lookup v ranges
     = do ce <- lookupVarEnv v
          appendStmt [cstm|if ($ce < $int:l || $ce > $int:h) { $ce = $int:l; }|]
     | otherwise = return ()


print_vars :: [EId] -> Exp -> Exp
print_vars [] e = e
print_vars (v:vs) e
  = let p = eSeq Nothing
             (ePrint Nothing False
                       (eVal Nothing TString (VString $ name v ++ ":")))
             (ePrint Nothing True  (eVar Nothing v))
    in eSeq Nothing p (print_vars vs e)

cg_print_vars :: DynFlags -> [EId] -> Cg ()
cg_print_vars dflags vs
  = do { _ <- codeGenExp dflags $ print_vars vs (eVal Nothing TUnit VUnit)
       ; return ()
       }




genLUTLookup :: DynFlags
             -> Map EId Range
             -> [EId]             -- ^ Input variables
             -> ([EId],Maybe EId)-- ^ Output variables incl. possibly a
                                    --   separate result variable!
             -> C.Exp               -- ^ LUT table
             -> Ty                  -- ^ Expression type
             -> Maybe EId          -- ^ If set, store the result here
             -> Exp                 -- ^ Original expression (just for debug)
             -> Cg C.Exp
genLUTLookup dflags ranges
             inVars (outVars,res_in_outvars) clut ety mb_resname _e = do
    idx <- freshVar "idx"
    appendDecl [cdecl| unsigned int $id:idx; |]

    appendStmt [cstm|$id:idx = 0; |]
    packIdx ranges inVars [cexp|$id:idx|] [cty|unsigned int|]

    case res_in_outvars of
      Just v ->
        do { unpackByteAligned outVars
                 [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--           ; cg_print_vars dflags outVars -- debugging

           ; vcexp <- lookupVarEnv v
           ; store_var mb_resname vcexp
           }
           -- DV: used to be ; return vcexp }
      Nothing
        | ety == TUnit
        -> do { unpackByteAligned outVars
                     [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--              ; cg_print_vars dflags outVars -- debugging

              -- DV: used to be; return [cexp|UNIT |] }
              ; store_var mb_resname [cexp|UNIT |]
              }
        | otherwise
        -> case mb_resname of
             Nothing ->
               do { res <- freshName "resx" ety Imm
                    -- if the result variable is an super-byte array we will not allocate space since the LUT
                    -- contains space for it already, rather we will return the address in the LUT
                  ; case ety of 
                      TArray (Literal l) ty | (ty /= TBit || l > 8) ->
                       do { appendDecl $ [cdecl| $ty:(codeGenTyAlg ety) $id:(name res); |]
                          ; respos <- unpackByteAligned outVars [cexp| (typename BitArrPtr) $clut[$id:idx]|]
                          ; appendStmt $ [cstm| $id:(name res) = 
                                ($ty:(codeGenTy ety)) & $clut[$id:idx][$int:respos]; |] 
                          ; return $ [cexp| $id:(name res) |]
                          }
                      _ -> 
                       do { codeGenDeclGroup (name res) ety >>= appendDecl
                          ; extendVarEnv [(res,[cexp|$id:(name res)|])] $
                            unpackByteAligned (outVars ++
                               [res]) [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--                        ; cg_print_vars dflags outVars -- debugging

                          ; return $ [cexp| $id:(name res) |]
                          }
                  }
             Just res ->
               do { extendVarEnv [(res,[cexp|$id:(name res)|])] $
                    unpackByteAligned (outVars ++
                       [res]) [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--                ; cg_print_vars dflags outVars -- debugging

                  ; return $ [cexp|UNIT|]
                  }

  where store_var Nothing ce_res
          = return ce_res
        store_var (Just res) ce_res
          = do { assignByVal ety ety [cexp|$id:(name res)|] ce_res
               ; return [cexp|UNIT|]
               }

genLocalVarInits :: DynFlags -> [EId] -> Cg ()
genLocalVarInits dflags vs
  = do { ds <- mapM (\v -> codeGenDeclGroup (name v) (nameTyp v)) vs
       ; appendDecls ds }

