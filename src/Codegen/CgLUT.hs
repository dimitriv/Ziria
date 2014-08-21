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
-- import System.Directory(getTemporaryDirectory)
-- import System.IO (IOMode(..),
--                   hClose,
--                   hGetContents,
--                   openFile,
--                   openTempFile)
import Text.PrettyPrint.Mainland

import LUTAnalysis

import qualified Data.Hashable as H

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

packIdx :: Map Name Range    -- Ranges
                  -> [VarTy] -- Variables
                  -> C.Exp   -- A C expression for the index variable
                  -> C.Type  -- The actual index type (typically unsigned int)
                  -> Cg ()
packIdx ranges vs tgt tgt_ty = go vs 0
  where go [] _ = return ()
        go ((v,ty):vs) pos
         | isArrTy ty 
         = do { w <- varBitWidth ranges v ty
              ; let mask = 2^w - 1
              ; (_,varexp) <- lookupVarEnv v
              ; z <- genSym "__z" 
              ; appendDecl $ [cdecl| $ty:tgt_ty * $id:z;|]
              ; appendStmt $ [cstm| $id:z = ($ty:tgt_ty *) $varexp; |]
              ; appendStmt $ [cstm| $tgt |= ((* $id:z) & $int:mask) << $int:pos; |]
              -- ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (pos+w) }
         | otherwise
         = do { w <- varBitWidth ranges v ty
              ; let mask = 2^w - 1
              ; (_,varexp) <- lookupVarEnv v
              ; appendStmt $ [cstm| $tgt |= ((($ty:tgt_ty) $varexp) & $int:mask) << $int:pos; |]
              -- ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) & $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (pos+w) }

unpackIdx :: [VarTy] -- Variables
          -> C.Exp   -- A C expression representing the index
          -> C.Type  -- The index type (typically unsigned int)
          -> Cg ()
unpackIdx xs src src_ty = go xs 0
  where go [] _ = return ()
        go ((v,ty):vs) pos
         | TArr basety alen <- ty
         , let bytesizeof = tySizeOf_C ty
         = do { (_,varexp) <- lookupVarEnv v
              ; w <- tyBitWidth ty 

              ; let mask = 2^w - 1

              ; tmpname_aux <- freshName "tmp_aux"
              ; tmpname <- freshName "tmp"

              ; appendDecl [cdecl| $ty:src_ty $id:(name tmpname_aux) = ($src >> $int:pos); |]
              
              ; appendDecl [cdecl| $ty:src_ty $id:(name tmpname) = $id:(name tmpname_aux) & $int:mask; |]
              ; appendStmt [cstm| memcpy((void *) $varexp, (void *) & ($id:(name tmpname)), $bytesizeof);|]

              ; go vs (pos+w) }
         | otherwise 
         = do { (_,varexp) <- lookupVarEnv v
              ; w <- tyBitWidth ty
              ; let mask = 2^w - 1  
              ; appendStmt $ [cstm| $varexp = (($src >> $int:pos)) & $int:mask; |]
              -- ; appendStmt $ [cstm| bitArrRead($src,$int:pos,$int:w, (typename BitArrPtr) & $varexp); |] 
              ; go vs (pos+w) }



packByteAligned :: Map Name Range -- Ranges
               -> [VarTy]         -- Variables
               -> C.Exp           -- A C expression of type BitArrPtr
               -> Cg ()
packByteAligned ranges vs tgt = go vs 0
  where go [] _ = return ()
        go ((v,ty):vs) pos
         | isArrTy ty 
         = do { w <- varBitWidth ranges v ty
              ; (_,varexp) <- lookupVarEnv v
              ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (byte_align (pos+w)) } -- Align for next write!
         | otherwise
         = do { w <- varBitWidth ranges v ty
              ; (_,varexp) <- lookupVarEnv v
              ; appendStmt $ [cstm| bitArrWrite((typename BitArrPtr) & $varexp, $int:pos, $int:w, $tgt); |]
              ; go vs (byte_align (pos+w)) }  -- Align for next write!

byte_align n = ((n+7) `div` 8) * 8

unpackByteAligned :: [VarTy] -- Variables
                   -> C.Exp   -- A C expression of type BitArrPtr
                   -> Cg ()
unpackByteAligned xs src = go xs 0
  where go [] _ = return ()
        go ((v,ty):vs) pos
         | isArrTy ty 
         = do { (_,varexp) <- lookupVarEnv v
              ; w <- tyBitWidth ty
              ; appendStmt $ [cstm| memcpy((void *) $varexp, (void *) & $src[$int:(pos `div` 8)], $int:(byte_align w `div` 8));|]
              --; appendStmt $ [cstm| bitArrRead($src,$int:pos,$int:w, (typename BitArrPtr) $varexp); |] 
              ; go vs (byte_align (pos+w)) } -- Align for next read!
         | otherwise 
         = do { (_,varexp) <- lookupVarEnv v
              ; w <- tyBitWidth ty 
              ; appendStmt $ [cstm| blink_copy((void *) & $varexp, (void *) & $src[$int:(pos `div` 8)], $int:(byte_align w `div` 8));|]
              --; appendStmt $ [cstm| bitArrRead($src,$int:pos,$int:w, (typename BitArrPtr) & $varexp); |] 
              ; go vs (byte_align (pos+w)) } -- Align for next read!


-- csrcPathPosix :: DynFlags -> FilePath
-- csrcPathPosix dflags = head [ path | CSrcPathPosix path <- dflags]

-- csrcPathNative :: DynFlags -> FilePath
-- csrcPathNative dflags = head [ path | CSrcPathNative path <- dflags]


codeGenLUTExp_Mock :: DynFlags -> Maybe SourcePos -> Map Name Range -> Exp Ty -> Cg C.Exp
codeGenLUTExp_Mock dflags loc r  e
  = do { (inVars, outVars, allVars) <- inOutVars [] r e
       ; _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                               eVal Nothing TString (VString (show loc))
       ; _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                                  eVal Nothing TString (VString "INVARS")
       ; cg_print_vars dflags inVars 
       ; _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                                  eVal Nothing TString (VString "OUTVARS")
       ; r <- codeGenExp dflags e
       ; cg_print_vars dflags outVars
       ; return r 
       }


codeGenLUTExp :: DynFlags
              -> [(Name,Ty,Maybe (Exp Ty))]
              -> Map Name Range
              -> Exp Ty
              -> Maybe Name -- If set then use this specific name for 
                            -- storing the final result
              -> Cg C.Exp
codeGenLUTExp dflags locals_ ranges e mb_resname 
  = case shouldLUT dflags locals ranges e of
      Left err -> 
        do { verbose dflags $ text "Asked to LUT un-LUTtable expression:" <+> string err </> nest 4 (ppr e)
           ; codeGenExp dflags e }
      Right True  -> lutIt mb_resname
      Right False -> 
        do { verbose dflags $ 
             text "Asked to LUT an expression we wouldn't normally LUT:" </> 
             nest 4 (ppr e)
           ; lutStats <- calcLUTStats locals ranges e
           ; if lutTableSize lutStats >= aBSOLUTE_MAX_LUT_SIZE
             then do { verbose dflags $ 
                       text "LUT way too big!" </> 
                         fromJust (pprLUTStats dflags locals ranges e)
                     ; codeGenExp dflags e }
             else lutIt mb_resname }
  where
    -- TODO: Document why this not the standard size 
    -- but something much bigger? (Reason: alignment)
    aBSOLUTE_MAX_LUT_SIZE :: Integer
    aBSOLUTE_MAX_LUT_SIZE = 1024*1024

    locals :: [VarTy]
    -- TODO: local initializers are ignored?? Potential bug
    locals = [(v,ty) | (v,ty,_) <- locals_]

    lutIt :: Maybe Name -> Cg C.Exp
    lutIt mb_resname = do
        (inVars, outVars, allVars) <- inOutVars locals ranges e

        verbose dflags $ 
          text "Creating LUT for expression:" </> nest 4 (ppr e) </> 
            nest 4 (text "Variable ranges:" </> pprRanges ranges) </> 
               fromJust (pprLUTStats dflags locals ranges e)

        let resultInOutVars 
             | Just v <- expResultVar e
             , v `elem` map fst outVars = Just v
             | otherwise = Nothing

        let h = H.hash (show e)
        -- liftIO $ putStrLn $ "Hash = " ++ show (H.hash (show e))
        hs <- getLUTHashes
        let gen_lut_action 
              = genLUT dflags ranges inVars (outVars,resultInOutVars) 
                       allVars locals e
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


{-
        _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                                 eVal Nothing TString (VString (show (expLoc e)))

        _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                                 eVal Nothing TString (VString "INVARS")

        cg_print_vars dflags inVars 

        _ <- codeGenExp dflags $ ePrint Nothing TUnit True $ 
                                 eVal Nothing TString (VString "OUTVARS")
-}

        genLUTLookup dflags ranges 
                        inVars (outVars,resultInOutVars) 
                        clut (info e) mb_resname e

genLUT :: DynFlags -- ^ Flags
       -> Map Name Range
       -> [VarTy]               -- ^ Input variables
       -> ([VarTy], Maybe Name) -- ^ Output variables + result if not in outvars
       -> [VarTy]  -- ^ All used variables
       -> [VarTy]  -- ^ Local variables
       -> Exp Ty   -- ^ Expression to LUT
       -> Cg LUTGenInfo
genLUT dflags ranges inVars (outVars, res_in_outvars) allVars locals e = do
    inBitWidth <- varsBitWidth ranges inVars    
    cinBitType <- lutIndexTypeByWidth inBitWidth
                            
    -- 'clut' is the C variable that will hold the LUT.
    clut <- genSym "clut"
    -- 'clutentry' is the C variable to hold one entry in the lut
    clutentry <- genSym "clutentry"

    -- 'cidx' is the C variable that we use to index into the LUT.
    cidx <- genSym "idx"

    -- 'clutgen' is the generator function for this LUT
    clutgen <- genSym "clut_gen"
    
    (defs, (decls, stms, outBitWidth))
        <- collectDefinitions $ 
           inNewBlock $ 
           -- Just use identity code generation environment
           extendVarEnv [(v, (ty, [cexp|$id:(name v)|])) | (v,ty) <- allVars] $
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

              ; mapM_ (\(v,_) -> ensureInRange v) inVars
              ; ce <- codeGenExp dflags e
              ; (outBitWidth, outVarsWithRes, result_env) <- 
                  if (isJust res_in_outvars || info e == TUnit) then
                    do { ow <- varsBitWidth_ByteAlign outVars
                         -- clutentry declaration and initialization to zeros
                         -- clutentry declaration and initialization to zeros
                       ; let entry_ty = TArr (Literal ow) TBit 
                       ; g <- codeGenArrVal clutentry entry_ty [VBit False]
                       ; appendDecl g
                       ; return (ow,outVars, []) }
                  else 
                    do { resval <- freshName "res";
                       ; codeGenDeclGroup (name resval) (info e) >>= appendDecl
                       ; let outVarsWithRes = outVars++[(resval,info e)]
                       ; ow <- varsBitWidth_ByteAlign outVarsWithRes
                         -- clutentry declaration and initialization to zeros
                       ; let entry_ty = TArr (Literal ow) TBit 
                       ; g <- codeGenArrVal clutentry entry_ty [VBit False]

                       ; appendDecl g 
                       ; assignByVal (info e) (info e) 
                                [cexp|$id:(name resval)|] ce
                       ; reswidth <- tyBitWidth (info e) 
                       ; let renv = 
                              [(resval,(info e, [cexp|$id:(name resval)|]))]
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
    ensureInRange :: Name -> Cg ()
    ensureInRange v 
     | Just (Range l h) <- Map.lookup v ranges  
     = do (_, ce) <- lookupVarEnv v
          appendStmt [cstm|if ($ce < $int:l || $ce > $int:h) { $ce = $int:l; }|]
     | otherwise = return ()


print_vars :: [VarTy] -> Exp Ty -> Exp Ty
print_vars [] e = e
print_vars ((v,t):vs) e
  = let p = eSeq Nothing TUnit 
             (ePrint Nothing TUnit False 
                       (eVal Nothing TString (VString $ name v ++ ":")))
             (ePrint Nothing TUnit True  (eVar Nothing t v))
    in eSeq Nothing (info e) p (print_vars vs e)

cg_print_vars :: DynFlags -> [VarTy] -> Cg ()
cg_print_vars dflags vs 
  = do { _ <- codeGenExp dflags $ print_vars vs (eVal Nothing TUnit VUnit) 
       ; return ()
       }




genLUTLookup :: DynFlags 
             -> Map Name Range
             -> [VarTy]             -- ^ Input variables
             -> ([VarTy],Maybe Name)-- ^ Output variables incl. possibly a 
                                    --   separate result variable!
             -> C.Exp               -- ^ LUT table
             -> Ty                  -- ^ Expression type 
             -> Maybe Name          -- ^ If set, store the result here
             -> Exp Ty              -- ^ Original expression (just for debug)
             -> Cg C.Exp
genLUTLookup dflags ranges 
             inVars (outVars,res_in_outvars) clut ety mb_resname _e = do
    idx <- genSym "idx"
    appendDecl [cdecl| unsigned int $id:idx; |]

    appendStmt [cstm|$id:idx = 0; |]
    packIdx ranges inVars [cexp|$id:idx|] [cty|unsigned int|]

    case res_in_outvars of
      Just v -> 
        do { unpackByteAligned outVars 
                 [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--           ; cg_print_vars dflags outVars -- debugging

           ; (_,vcexp) <- lookupVarEnv v
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
               do { res <- freshName "resx" 
                  ; codeGenDeclGroup (name res) ety >>= appendDecl
                  ; extendVarEnv [(res,(ety,[cexp|$id:(name res)|]))] $
                    unpackByteAligned (outVars ++ 
                       [(res,ety)]) [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--                  ; cg_print_vars dflags outVars -- debugging

                  ; return $ [cexp| $id:(name res) |] 
                  }
             Just res -> 
               do { extendVarEnv [(res,(ety,[cexp|$id:(name res)|]))] $
                    unpackByteAligned (outVars ++ 
                       [(res,ety)]) [cexp| (typename BitArrPtr) $clut[$id:idx]|]

--                  ; cg_print_vars dflags outVars -- debugging

                  ; return $ [cexp|UNIT|] 
                  }
    
  where store_var Nothing ce_res
          = return ce_res
        store_var (Just res) ce_res
          = do { assignByVal ety ety [cexp|$id:(name res)|] ce_res
               ; return [cexp|UNIT|]
               }

genLocalVarInits :: DynFlags -> [VarTy] -> Cg ()
genLocalVarInits dflags vs
  = do { ds <- mapM (\(v,ty) -> codeGenDeclGroup (name v) ty) vs
       ; appendDecls ds }

