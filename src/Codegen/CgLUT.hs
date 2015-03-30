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

{----------------------------------------------------------------------------
   Infrastructure 
----------------------------------------------------------------------------}

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
  -- TODO: by-ref variables and struct pointers?
  if isArrayTy (nameTyp v)
      then [cexp| (typename BitArrPtr) $varexp    |]
      else [cexp| (typename BitArrPtr) (& $varexp)|]


cgBreakDown :: Int -> C.Exp -> [C.Exp]
-- ^ Breakdown to 64,32,16,8 ... (later we should also add SSE 128/256)
cgBreakDown 8  ptr = [ptr]
cgBreakDown 16 ptr = [[|(uint16 *) ptr|]]
cgBreakDown 32 ptr = [[|(uint32 *) ptr|]]
cgBreakDown 64 ptr = [[|(uint64 *) ptr|]]
cgBreakDown n  ptr
  | n - 64 > 0 
  = [|(uint64 *) ptr|] : cgBreakDown (n-64) [cexp| ptr + 8 |]
  | n - 32 > 0 
  = [|(uint32 *) ptr|] : cgBreakDown (n-32) [cexp| ptr + 4 |] 
  | n - 16 > 0 
  = [|(uint16 *) ptr|] : cgBreakDown (n-16) [cexp| ptr + 2 |]
  | n - 8 > 0
  = [ptr]
  | otherwise = []

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
    in sequence_ (appendStmt . mk_stmt) $ zip3 vptrs mptrs lptrs

cgBitArrRead :: C.Exp -> Int -> Int -> C.Exp -> Cg ()
-- ^ Pre: pos and len are multiples of 8; src, tgt are of type BitArrPtr
cgBitArrRead src_base pos len tgt
  | len > 128
  = appendStmt $ 
    [cstm| blink_copy((void *) $tgt, $int:byte_len, (void *) $src;|]
  | otherwise
  = sequence_ (appendStmt . mk_stmt) $ zip src_ptr tgt_ptrs
  where
    src_ptrs      = cgBreakDown len src
    tgt_ptrs      = cgBreakDown len tgt
    mk_stmt (s,t) = appendStmt [cstm| *t = *s;|]
    sidx          = pos `div` 8
    src           = [cexp| & ($src[sidx])|]
    byte_len      = (len+7) `div` 8


{----------------------------------------------------------------------------
   Packing index variables (see also Note [LUT Packing Strategy])
----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------
   Unpacking index variables (see also Note [LUT Packing Strategy])
----------------------------------------------------------------------------}
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
       appendStmt $ 
         [cstm|bitArrWrite($idx_ptr,$int:(lidx*base_w),$slice_w,$vptr);|]
       let new_idx = idx `cExpShR` slice_w
       appendStmt $ [cstm| $idx = $new_idx;|]
  | otherwise
  = do w    <- inVarBitWidth pkg v
       vptr <- varToBitArrPtr v
       appendStmt $
         [cstm|bitArrWrite((BitArrPtr) & $idx,0,$int:w,$varexp_ptr);|]
       let new_idx = idx `cExpShR` w
       appendStmt $ [cstm| $idx = $new_idx;|]
  where idx_ptr      = [cexp| (BitArrPtr) & $idx |]

{----------------------------------------------------------------------------
   Packing (byte-aligned) output variables (see Note [LUT Packing Strategy])
----------------------------------------------------------------------------}

packOutVars :: VarUsePkg          -- ^ Usage info
            -> [(EId, Maybe EId)] -- ^ Output vars and assign-masks 
                                 --  ^ NB: Masks are of type BitArrPtr
            -> C.Exp              -- ^ Of BitArrPtr type
            -> Cg C.Exp           -- ^ Final BitArrPtr for final result
-- ^ NB: This is not perf critical as it is only used for LUT generation.
packOutVars pkg vs tgt = do
  final_bit_idx <- go vs 0
  let final_byte_idx = (final_bit_idx + 7) `div` 8
  return $ [cexp| & tgt[$int:final_byte_idx]|]

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
    Just v' -> do 
      vexp' <- lookupVarEnv v'
      appendStmt [cstm| bitArrWrite(vexp',$int:(pos+w),$int:w,$tgt);|]
    Nothing -> return ()
  -- | Return new position
  return (pos+total_width)

{----------------------------------------------------------------------------
   Unpacking (byte-aligned) output vars (see aNote [LUT Packing Strategy])
----------------------------------------------------------------------------}

unpackOutVars :: VarUsePkg          -- ^ Usage info
              -> [(EId, Maybe EId)] -- ^ Output vars and assign-masks
              -> C.Exp              -- ^ Already of BitArrPtr type
              -> Cg C.Exp           -- ^ Final BitArrPtr for final result
unpackOutVars pkg vs src = 
  final_bit_idx <- go vs n
  let final_byte_idx = (final_bit_idx + 7) `div` 8
  return $ [cexp| & src[$int:final_byte_idx]|]

  where go [] n      = return n
        go (v:vs) pos = unpack_out_var pkg v src pos >>= go vs

unpack_out_var :: VarUsePkg
               -> (EId, Maybe EId)
               -> C.Exp
               -> Int
               -> Cg Int
unpack_out_var pkg (v, Nothing) src pos = do
  vptr <- varToBitArrPtr v
  w    <- outVarBitWidth pkg v -- ^ Post: w is multiple of 8
  cgBitArrRead src pos w vptr
  return (pos+w)

unpack_out_var pkg (v, Just {}) src pos = do
  vptr    <- varToBitArrPtr v
  total_w <- outVarBitWidth pkg v
  let w  = total_w / 2
  let mask_ptr = [cexp| & src[$int:((pos+w) `div` 8)]|]
  cgBitArrLUTMask vptr mask_ptr src w
  return (pos + total_w)


{----------------------------------------------------------------------------
   Compile an epxression to LUTs
----------------------------------------------------------------------------}

codeGenLUTExp :: DynFlags
              -> LUTStats  -- ^ LUT stats for the expression to LUT-compile
              -> Exp       -- ^ The expression to LUT-compile
              -> Maybe EId -- ^ If set then use this variable as output
              -> Cg C.Exp
codeGenLUTExp dflags stats e mb_resname

    -- | We simply cannot LUT, fullstop.
  | Left err <- lutShould stats
  = do let msg = text "Compiling without LUT."
       verbose dflags $ cannot_lut err msg
       codeGenExp dflags e

    -- | Below we were forced to LUT although analysis recommends we don't
  | Right False <- lutShould stats
  , lutTableSize stats >= aBSOLUTE_MAX_LUT_SIZE
  = do let msg = text "LUT size way too large, compiling without LUT!"
       verbose dflags $ cannot_lut empty msg
       codeGenExp dflags e

    -- | Otherwise just LUT it
  | _otherwise = lutIt mb_resname

  where
    cannot_lut d what_next
      = vcat [ text "Asked to LUT an expression we would not LUT." <+> d
             , nest 4 (ppr e)
             , text "At location" <+> ppr (expLoc e)
             , what_next ]

    aBSOLUTE_MAX_LUT_SIZE :: Integer
    aBSOLUTE_MAX_LUT_SIZE = 1024 * 1024

    hashGenLUT :: DynFlags -> LUTStats -> Exp -> Cg LUTGenInfo
    -- ^ Call genLUT, but use existing LUT if we have compiled
    -- ^ this expression before to a LUT.
    hashGenLUT dflags stats e
      | isDynFlagSet dflags NoLUTHashing = genLUT dflags stats e
      | otherwise
      = do hs <- getLUTHashes
           let h = H.hash (show e)
           case lookup h hs of
             Just clut -> do
               verbose dflags $ text "Expression to LUT is already lutted!"
               return clut
             Nothing -> do
               verbose dflags $ text "Invoking LUT generation (genLUT)"
               lgi <- genLUT dflags stats e
               setLUTHashes $ (h,lgi):hs
               return lgi

    lutIt :: Maybe EId -> Cg C.Exp
    lutIt mb_resname = do

      verbose dflags $
        vcat [ text "Creating LUT for expression:"
             , nest 4 $ ppr e
             , nest 4 $ vcat [ text "LUT stats"
                             , ppr stats ] ]

      -- Generate code that will generate the LUT at init
      -- time and return the LUT table name.
      clut <- hashGenLUT dflags stats e

      -- Generate LUT lookup code.
      genLUTLookup dflags stats clut (ctExp e) mb_resname


{----------------------------------------------------------------------------
   Generate the code for looking up a LUT value
----------------------------------------------------------------------------}

genLUTLookup :: DynFlags
             -> LUTStats
             -> LUTGenInfo -- ^ LUT table information
             -> C.Exp      -- ^ LUT table
             -> Ty         -- ^ Expression type
             -> Maybe EId  -- ^ If set, store result here
             -> Cg C.Exp
-- ^ Returns () if result has been already stored in mb_resname,
-- ^ otherwise it returns the actual expression.
genLUTLookup dflags stats clut ety mb_resname = do
   
   -- | Declare local index variable
   idx <- freshVar "idx"
   idx_ty <- lutIndexTypeByWidth (lutInBitWidth stats)
   appendDecl [cdecl| $idx_ty $id:idx;|]
   appendStmt [cstm| $id:idx = 0;|]

   -- | Pack input variables to index
   let vupkg = lutVarUsePkg stats
   packIdx vupkg (vu_invars vupkg) [cexp|$id:idx|] idx_ty

    -- | LUT lookup and unpack output variables
   let outvars = lgi_masked_outvars clut   
   let lkup_val = [cexp|(typename BitArrPtr) $clut[$id:idx]|]
   cres <- unpackOutVars vupkg outvars lkup_val 

   -- | The result is either an existing output variable or @cres@
   result <- case lutResultInOutVars stats of
     Just v  -> lookupVarEnv v
     Nothing -> do
       let cty = codeGenTy ety
           cres_well_typed
             | TUnit     <- ety = [cexp|UNIT|]
             | TArray {} <- ety = [cexp|   ($ty:cty) cres  |]
             | other            = [cexp| * (($ty:cty) cres)|]
           return cres_well_typed

   -- | Store the resul if someone has given us a location 
   store_var mb_resname result

   where
     store_var Nothing cres = return cres
     store_var Just res_var cres = do 
        cres_var <- lookupVarEnv res_var
        assignByVal ety ety [cexp|$cres_var|] cres
        return [cexp|UNIT|]


{----------------------------------------------------------------------------
   Generate the code for generating a LUT 
----------------------------------------------------------------------------}

genLUT :: DynFlags
       -> LUTStats       -- ^ LUT stats and info
       -> Exp            -- ^ The expression to LUT
       -> Cg LUTGenInfo  -- ^ Generated LUT handles
genLUT dflags stats e = do
   let vupkg = lutVarUsePkg stats
       ety   = ctExp e
       cty   = codeGenTy ety
       loc   = expLoc e

   clut <- freshVar "clut"
   -- | Temporary variable for one lut entry
   clutentry <- freshVar "clutentry"
   let clutentry_ty = TArray (Literal (lutOutBitWidth stats)) TBit
   -- | LUT index
   cidx <- freshVar "idx"
   let cidx_ty = lutIndexTypeByWidth (lutInBitWidth stats)
   -- | Function name that will populate this LUT
   clutgen <- freshVar "clut_gen"
   -- | Generate mask variables for output
   mask_eids <- mapM genOutVarMask (vu_outvars vupkg)
   (defs,(decls,stms,_)) <- do
      collectDefinitions $ inNewBlock $
      genLocalVarInits dflags (vu_allvars vupkg) $
      genLocalMaskInits dflags mask_eids $ do 
         -- | Unpack the index into the input variables
         unpackIdx vupkg (vu_invars vupkg) cidx_ty
         -- | Debug the LUT
         cgDebugLUTIdxPack cidx_ty (vu_invars vupkg) loc
         -- | Initialize clutentry
         let bit0 = eVal Nothing TBit (VBit False)
         codeGenArrVal clutentry clutentry_ty [bit0]
         -- | Instrument e and compile
         e' <- lutInstrument mask_eids e
         ce <- codeGenExp dflags e'
         clut_fin <- packOutVars vupkg mask_eids [cexp| $id:clutentry|]
         -- | The result is either an existing output variable or @clut_fin@
         result <- case lutResultInOutVars stats of
           Just v  -> return ()
           Nothing
             | TArray {} <- ety
             = assignByVal ety ety [cexp| ($ty:cty) clut_fin |] ce
             | otherwise
             = assignByVal ety ety [cexp| *(($ty:cty) clut_fin)|] ce

   -- | make lut entry be 2-byte aligned
   let lutEntryByteLen = ((((outBitWidth + 7) `div` 8) + 1) `div` 2) * 2
   let idxLen = (1::Word) `shiftL` inBitWidth
       lutbasety = namedCType $ "calign unsigned char"
       clutDecl = [cdecl| $ty:lutbasety
                              $id:clut[$int:idxLen][$int:lutEntryByteLen];|]
   cbidx <- freshVar "bidx"
   let clutgen_def
         = [cedecl|void $id:clutgen() {
               for($ty:cidx_ty $id:cidx = 0;
                        $id:cidx < $int:idxLen;
                        ($id:cidx)++) 
               {
                  $decls:decls
                  unsigned int $id:cbidx;
                  $stms:stms                 
                  for ($id:cbidx = 0; 
                          $id:cbidx < $int:(lutEntryByteLen); $id:cbidx++) 
                  { 
                      $id:clut[$id:cidx][$id:cbidx] = 
                                      $id:clutentry[$id:cbidx]; 
                  }
               }}
           |]
   appendTopDecl clutDecl
   appendTopDefs defs
   appendTopDef clutgen_def
   return $ LUTGenInfo { lgi_lut_var = [cexp|$id:clut|]
                       , lgi_lut_gen = [cstm|$id:clutgen();|]
                       , lgi_lut_masked_outvars = mask_list 
                       }

genOutVarMask :: DynFlags -> EId -> Cg (EId, Maybe EId)
-- ^ Generate a new output mask variable
genOutVarMask dfs x
  | isArrayTy (nameTyp x) || isStructTy (nameTyp x)
  , let bitarrty = TArray (outVarBitWidth (nameTyp x)) TBit 
  = do x_mask  <- freshName (name x ++ "_mask") bitarrty Mut
       return (x, Just x_mask)
  | otherwise 
  = return (x, Nothing)

genLocalMaskInits :: DynFlags -> [(EId, Maybe EId)] -> Cg a -> Cg a
-- ^ Declare the mask variables, initialize them and extend the environment
genLocalMaskInits dfs mask_vars
  let new_bind (x,Nothing) = return Nothing
      new_bind (x,Just mx) = do
         mcx <- freshVar (name mx)
         g <- codeGenArrVal mcx (nameTyp mx) [bIT0]
         appendDecl g
         return $ Just (mx,mcx)
  var_env <- catMaybes <$> mapM new_bind mask_vars
  extendVarEnv var_env action


genLocalVarInits :: DynFlags -> [EId] -> Cg a -> Cg a
-- ^ Declare local variables and extend the environment
genLocalVarInits dflags variables action = do 
  let new_bind v = do 
         cv <- freshVar (name v); 
         codeGenDeclGroup cv (nameTyp v) >>= appendDecl
         return (v,[cexp|$id:cv|])
  var_env <- mapM new_bind variables
  extendVarEnv var_env action

-- | Debugging lut index packing
cgDebugLUTIdxPack :: C.Exp            -- ^ original index
                  -> C.Type           -- ^ type of index
                  -> VarUsePkg        -- ^ var use info
                  -> Maybe SourcePos  -- ^ location
                  -> Cg ()
cgDebugLUTIdxPack cidx cidx_ty vupkg loc = do
   dbg_cidx <- freshVar "dbg_idx"
   appendDecl [cdecl| $ty:cidx_ty $id:dbg_cidx;|]
   packIdx vupkg (vu_invars vupkg) [cexp|$id:dbg_cidx|] cidx_ty
   appendStmt [cstm| 
     if ($id:cidx != $id:dbg_cidx) {
        printf("Fatal bug in LUT generation: packIdx/unpackIdx mismatch.\n");
        printf("Location: %s", $string:(show (expLoc e)));
        exit(-1); } |]


{----------------------------------------------------------------------------
   Instrument an expression to update the assign-masks
----------------------------------------------------------------------------}

bIT0 :: Exp
bIT0 = eVal Nothing TBit (VBit False)

bIT1 :: Exp
bIT1 = eVal Nothing TBit (VBit True)


bitArrRng :: Ty -> LenInfo -> Int
-- ^ Return the bit width of this array slice
bitArrRng base_ty LISingleton = tyVarBitWidth base_ty
bitArrRng base_ty LIRange n   = n * tyVarBitWidth base_ty

fldBitArrRng :: [(FldName,Ty)] -> FldName -> (Int, Int)
-- ^ Return the bit interval that this field 
-- ^ corresponds to in a bit array mask
fldBitArrRng vs the_fld = go vs 0
  where go _ [] = error "fldBitArrRng: not in struct!"
        go n ((f,ty):rest)
          | f == the_fld = (n, tyVarBitWidth ty)
          | otherwise    = go (n + tyVarBitWidth ty) rest

eBitArrSet :: EId -> Exp -> Int -> Exp
-- ^ Set some bits in this bit array
eBitArrSet bitarr start width 
  = eArrWrite Nothing () start width (replicate width bIT1)

eMultBy :: Exp -> Int -> Exp
eMultBy e n = eBinOp loc () Mult e (eVal loc () ety (VInt ni))
  where ety = ctExp e
        loc = ctLoc e
        ni  = fromIntegral n

eArrWriteUpdateMask :: EId                -- ^ the array
                    -> [(EId, Maybe EId)] -- ^ the mask map
                    -> Exp                -- ^ start index (pre: pure)
                    -> LengthInfo         -- ^ length info
                    -> [Exp]              -- ^ assignment
eArrWriteUpdateMask x mask_map estart len
  | TArray _ basety <- nameTyp x
  , Just mask_var <- lookup x mask_map
  , let loc   = expLoc e
  , let start = estart `eMultBy` tyBitWidth basety
  = [eBitArrSet mask_var start (bitArrRng basety len)]
  | otherwise 
  = []

eFldWriteUpdateMask :: EId                -- ^ the struct
                    -> [(EId, Maybe EId)] -- ^ the mask map
                    -> FldName            -- ^ field name
                    -> [Exp]              -- ^ assignment
eFldWriteUpdateMask x mask_map fld
  | TStruct _ fltys <- nameTyp x
  , Just mask_var <- lookup x mask_map
  , let (i,j) = fldBitArrRng fltys fld
  = [eBitArrSet mask_var (eVal Nothing () tint (VInt i)) j]
  | otherwise 
  = []

-- | Main expression instrumentation
lutInstrument :: [(EId, Maybe EId)] -> Exp -> Cg Exp
lutInstrument mask_eids e = mapExpM return return (do_asgn . unExp)
  where
    loc = expLoc e
    do_asgn (EAssign elhs erhs) = do
      let lval = parse_deref elhs
      (bnds, lval') <- liftLValEffects lval
      es <- procPureLVal lval'
      eLetMany bnds <$>
        eSeqs (eAssign loc () (derefToExp lval') erhs : es)

    do_asgn (EArrWrite earr estart len _rhs) = do
      let lval = parse_deref (eArrRead loc () earr estart len)
      (bnds,lval') <- liftLValEffects lval
      es <- procPureLVal lval'
      let (GDArr loc () de estart' len') = lval
      eLetMany bnds <$>
        eSeqs (eArrWrite loc () (derefToExp de) estart' len' rhs : es)

    do_asgn e0 = MkExp e0 loc ()

    parse_deref = fromJust . isMutGDerefExp 

procPureLVal :: [(EId, Maybe EId)] -- ^ Mask map
             -> GDerefExp Ty ()    -- ^ LVal
             -> Cg [Exp]           -- ^ Assignments
procPureLVal mask_map (GDVar loc () x)
  | Just mvar <- lookup x mask_map
  , let start = eVal Nothing () tint (VInt 0)
  , let len   = tyBitWidth $ nameTyp x
  = [eBitArrSet mask_var start len]
  | otherwise = []
procPureLVal mask_map (GDArr loc () (GDVar _loc () x) estart len)
  = eArrWriteUpdateMask x mask_map estart len
procPureLVal mask_map (GDArr loc () d _ _)
  = procPureLVal d
procPureLVal mask_map (GDProj loc () (GDVar _loc () x) fld)
  = eFldWriteUpdateMask x mask_map fld
procPureLVal mask_map (GDProj loc () d fld)
  = procPureLVal mask_map d
procPureLVal mask_map (GDNewArray {})  = []
procPureLVal mask_map (GDNewStruct {}) = []


liftLValEffects :: GDerefExp Ty () -> Cg ([(EId,Exp)],GDerefExp Ty ())
liftLValEffects d@(GDVar {}) = return ([],d)
liftLValEffects (GDProj loc a de fld) = do
  (bnds,de') <- liftLValEffects de
  return (bnds, GDProj loc a de' fld)
liftLValEffects (GDArr loc a de e len) = do
  nm <- freshName "n" (ctExp e) Imm
  let nmexp = eVar loc () nm
  (bnds,de') <- liftLValEffects de
  let bnds' = (nm,e) : bnds
  return (bnds', GDArr loc a de' nmexp len)


eLetMany :: [(EId,Exp)] -> Exp -> Exp
eLetMany [] e = e
eLetMany ((nm1:e1):bnds) e
  = eLet Nothing () nm1 AutoInline e1 (eLetMany bnds e)

eSeqs :: [Exp] -> Exp
eSeqs [e] = e
eSeqs (e1:es) = eSeq Nothing () e1 (eSeqs es)
