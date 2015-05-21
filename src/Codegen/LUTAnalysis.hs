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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module LUTAnalysis (

   -- | LUT statistics and decision if we should LUT or not
   LUTStats ( .. )
 , calcLUTStats

   -- | Densely packed input variable width
 , tyBitWidth, tyBitWidth_ByteAlign
 , inVarsBitWidth
 , inVarBitWidth
 , inArrSliceBitWidth
   -- | Byte-aligned packed output variable width and assign-masks

 , outVarsBitWidth -- ^ Total output variables and their masks 
 , outVarBitWidth  -- ^ Includes mask width
 , outVarMaskWidth -- ^ Only this variable's width
 ) where

import Opts
import AstExpr
import CgTypes

import Control.Applicative

import Text.PrettyPrint.HughesPJ 

import Outputable
import CtExpr

import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Error

import NameEnv ( neLookup )

import Analysis.DataFlow
import Analysis.RangeAnal 

import Data.Maybe ( isJust )

import Utils ( assert )

{------------------------------------------------------------------
  Bitwidths of various types needed for LUT
-------------------------------------------------------------------}

tyBitWidth :: Monad m => Ty -> m Integer
-- | This gives the /precise/ bitwidth of a type but the actual bit
-- width may be bigger, e.g. a Bit is declared as unsigned char, and
-- a bit array of 6 bits is a 1-element byte array.
tyBitWidth TUnit                   = return 0
tyBitWidth TBit                    = return 1 -- NB not 8
tyBitWidth TBool                   = return 1 -- NB not 8
tyBitWidth (TInt bw)               = return $ fromIntegral $ bwBitWidth bw
tyBitWidth TDouble                 = return 64
tyBitWidth (TArray (Literal n) ty) = tyBitWidth ty >>= \w -> return (fromIntegral n * w)
tyBitWidth t@(TStruct tn _flds)
  | tn == complexTyName   = return 64 -- NB: platform-dependent
  | tn == complex8TyName  = return 16 -- but ok with GCC and VS in
  | tn == complex16TyName = return 32 -- Cygwin and in VS
  | tn == complex32TyName = return 64

-- | For the moment we do not calculate bit width of
-- | arbitrary structs.  This means that expressions 
-- | that manipulate structs will not be lutted.
tyBitWidth ty
  = fail $ "Cannot calculate bit width of type " ++ show ty

tyBitWidth_ByteAlign :: Monad m => Ty -> m Integer
-- | Similar version but overshoots to align to byte boundaries
tyBitWidth_ByteAlign ty = do 
  w <- tyBitWidth ty
  return $ ((w + 7) `div` 8) * 8


{------------------------------------------------------------------
  LUT statistics
-------------------------------------------------------------------}

-- | LUT statistics
data LUTStats = LUTStats { 
     lutInBitWidth      :: Integer   -- ^ Index bitwidth
   , lutOutBitWidth     :: Integer   -- ^ Total output bitwidth (contains result!)
   , lutResultBitWidth  :: Integer   -- ^ Result bitwidth
   , lutTableSize       :: Integer   -- ^ Size needed (in bytes)
   , lutVarUsePkg       :: VarUsePkg -- ^ Var use info

     -- | If @lutResultInOutVars = Just v@ then 
     --   (a) @v@ is the result variable
     --   (b) and is in @vu_outvars lutVarUsePkg@
   , lutResultInOutVars :: Maybe EId
   , lutShould          :: Either String Bool -- ^ Should we LUT?
   } 
   deriving (Eq, Ord)

instance Outputable LUTStats where
  ppr s = vcat $
      [ pprVarUsePkg (lutVarUsePkg s)
      , text "   result bitsize:" <+> resdoc
      , text "    input bitsize:" <+> ppr (lutInBitWidth s)
      , text "   output bitsize:" <+> ppr (lutOutBitWidth s)
      , text "lut size in bytes:" <+> ppr (lutTableSize s)
      , text " should be lutted:" <+> should (lutShould s) 
      ]
   where
    should :: Either String Bool -> Doc
    should (Left err)    = text "No, because" <+> text err
    should (Right False) = text "No"
    should (Right True)  = text "Yes"
    resdoc = if isJust $ lutResultInOutVars s
             then text "included in output variables"
             else ppr (lutResultBitWidth s)

calcLUTStats :: DynFlags
             -> Exp -> IO (Either Doc LUTStats)
calcLUTStats dflags e = runErrorT $ do
  pkg <- inOutVars dflags e
  
  verbose dflags $ vcat [ text "-- calcLUTStats --"
                        , ppr e
                        , pprVarUsePkg pkg ]

  -- Input (LUT index) bitwidth
  inBitWidth <- inVarsBitWidth pkg
  
  -- Calculate output bitwidth
  let resultInOutVars
       | Just v <- expResultVar e
       , v `elem` vu_outvars pkg = Just v
       | otherwise               = Nothing
 
  (outBitWidth, resultBitWidth) <- do
      b1 <- outVarsBitWidth pkg
      if isJust resultInOutVars then return (b1,0)
      else do rbw <- tyBitWidth_ByteAlign (ctExp e)
              return (b1+rbw,rbw)

  let bytes  = lutBytes inBitWidth outBitWidth
      should = shouldLUT dflags outBitWidth bytes e

  return
    LUTStats { lutInBitWidth      = inBitWidth
             , lutOutBitWidth     = outBitWidth
             , lutResultBitWidth  = resultBitWidth
             , lutResultInOutVars = resultInOutVars

             , lutTableSize       = bytes
             , lutShould          = should
             , lutVarUsePkg       = pkg
             }
  where
    -- | How many bytes will this LUT take
    lutBytes :: Integer -- ^ Input width  (in bits)
             -> Integer -- ^ Output width (in bits)
             -> Integer -- ^ Table size   (in bytes)
    lutBytes inw outw =
      -- | num entries * bytes per entry
      assert "lutBytes" (inw >= 0) $
      (2^(fromIntegral inw :: Integer)) * ((fromIntegral outw + 7) `div` 8)

    -- | Does this expression return a variable? 
    expResultVar :: Exp -> Maybe EId
    expResultVar (MkExp (ESeq _ e2) _ _) = expResultVar e2
    expResultVar (MkExp (EVar v) _ ty)   = Just v
    expResultVar _                       = Nothing



{---------------------------------------------------------------------
  Determine if we should LUT
----------------------------------------------------------------------}

newtype LM a = LM { unLM :: ErrorT String (State LMState) a }
  deriving ( Functor, Applicative, Monad, MonadState LMState
           , MonadError String )
  
evalLM :: LM a -> LMState -> Either String a
evalLM m s = fst $ runState (runErrorT $ unLM m) s

data LMState = LMState { lmHasLoop          :: !Bool
                       , lmHasCall          :: !Bool
                       , lmHasLocalFunction :: !Bool
                       , lmHasLUT           :: !Bool
                       , lmOpCount          :: !Int
                       , lmHasBPerm         :: !Bool
                       }

mIN_OP_COUNT :: Int
mIN_OP_COUNT = 5

shouldLUT :: DynFlags 
          -> Integer  -- ^ LUT output bitwidth
          -> Integer -- ^ LUT total size in bytes
          -> Exp -> Either String Bool
shouldLUT dflags lut_outbitwidth lut_tablesize e = flip evalLM s0 $ do
  should e
  s <- get
  return $ and [ not (lmHasCall s)
               , not (lmHasLUT s)
               , lut_outbitwidth /= 0 && lut_outbitwidth > 2
                 -- ^ LUTting only if bitwidth of output is > 2 bits,
                 -- ^ otherwise probably it's not worth it
               , lmOpCount s > mIN_OP_COUNT
               , lut_tablesize <= maxLUTSize dflags
               , luttable_out_type (ctExp e)
               ]
  where
    -- | Non-nested arrays in LUT generation.
    --   There is no fundamental reason for this other than the fact that
    --   the implementation of LUT is simpler (avoids a complicated cast)
    --   when unpacking/packing the final result from the LUT.
    luttable_out_type (TArray _ ty) = non_array ty
    luttable_out_type _             = True 
    non_array (TArray {}) = False
    non_array _           = True

    s0 :: LMState
    s0 = LMState { lmHasLoop = False
                 , lmHasCall = False
                 , lmHasLocalFunction = False
                 , lmHasLUT  = False
                 , lmOpCount = 0
                 , lmHasBPerm = False
                 }

    should_many :: [Exp] -> LM ()
    should_many = mapM_ should

    should :: Exp -> LM ()
    should (MkExp e _ _) = go e

    go :: Exp0 -> LM ()
    go e@(EVal {})    = return ()
    go e@(EValArr {}) = return ()
    go e@(EVar {})    = return ()

    go (EUnOp _ e) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e

    go (EBinOp _ e1 e2) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should_many [e1, e2]

    go (EAssign e1 e2) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should_many [e1, e2]

    go (EArrRead e1 e2 _) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should_many [e1, e2]

    go (EArrWrite e1 e2 _ e3) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should_many [e1,e2,e3]

    go (EFor _ _ e1 e2 e3) = do
        modify $ \s -> s { lmHasLoop = True }
        should_many [e1, e2, e3]
        -- making sure that we LUT loops more often
        modify $ \s -> s { lmOpCount = max (lmOpCount s) (mIN_OP_COUNT+1) }

    go (EWhile e1 e2) = do
        modify $ \s -> s { lmHasLoop = True }
        should_many [e1, e2]
        -- making sure that we LUT loops more often
        modify $ \s -> s { lmOpCount = max (lmOpCount s) (mIN_OP_COUNT+1) }

    go (ELet _ _ e1 e2) =
        should_many [e1, e2] 

    go (ELetRef _ Nothing e2) =
        should e2
    go (ELetRef _ (Just e1) e2) =
        should_many [e1, e2]

    go (ESeq e1 e2) =
        should_many [e1, e2] 

    go (ECall _ es) = do
        modify $ \s -> s { lmHasCall = True }
        should_many es


    go (EIf e1 e2 e3) =
        should_many [e1,e2,e3]

    go (EPrint _ es) =
        should_many es

    go e@(EError {}) = 
        return ()

    go (ELUT _ e) = do
        modify $ \s -> s { lmHasLUT = True }
        should e

    go (EStruct tn tfs)
       = mapM_ (\(fn,fe) -> should fe) tfs

    go (EProj e _fn)
       = should e



{- | Note [LUT Packing Strategy] 
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
For input variables we choose the tightest possible packing.

  o Integer variables:
      We use the tightest range we find in VarUsePkg
  o Array variables:
      We use the tightest slice of the array we find VarUsePkg
  o Struct variables:
      We make the variable as input, but in the future we should
      consider doing the same thing as for arrays, that is, use
      only the fields that we read as input.
  o All other just get their tyBitWidth (in CgTypes) and we have
     to generate code that packs accordingly.

For output variables:

  o Array and struct output variables get *twice* the space
    required for a byte-aligned placement. The first part is an
    actual array but the second part is a mask where the non-zero
    entriescorrespond to the parts of the arrays that were modified
    by the expression to LUT.
  o All other variables take up their byte-aligned type width.

The reason that output variables are placed in byte-aligned positions
is to avoid expensive bit shifting operations during unpacking. We can
spare some space in the output of the LUT as the total size of the LUT
is 2^inputwidth * (outputwidth / 8).

-}

{---------------------------------------------------------------------
  Widths of output variables of LUTs, see note [LUT Packing Strategy]
----------------------------------------------------------------------}

-- | Output variables bitwidth. NB: byte-aligned!
outVarsBitWidth :: (Functor m, Monad m) => VarUsePkg -> m Integer
outVarsBitWidth pkg = outvars_bitwidth (vu_outvars pkg)

outVarBitWidth :: (Functor m, Monad m) => EId -> m Integer
-- | NB: Includes the mask width!
outVarBitWidth = outvar_bitwidth 

outvar_bitwidth :: (Functor m, Monad m) => EId -> m Integer
outvar_bitwidth x = go (nameTyp x) x
  where go t@(TArray {})  x = double <$> tyBitWidth_ByteAlign t
        go t@(TStruct {}) x = double <$> tyBitWidth_ByteAlign t
        go t otherwise      = tyBitWidth_ByteAlign t
        double n = 2*n

outVarMaskWidth :: Ty -> Maybe Integer
-- | Original type of variable, return the width 
-- of the mask if this type is mask-able.
outVarMaskWidth t@(TArray {})  = tyBitWidth_ByteAlign t
outVarMaskWidth t@(TStruct {}) = tyBitWidth_ByteAlign t
outVarMaskWidth t              = Nothing


outvars_bitwidth :: (Functor m, Monad m) => [EId] -> m Integer
outvars_bitwidth vartys = do
  sizes <- mapM outvar_bitwidth vartys
  return $ sum sizes


{---------------------------------------------------------------------
  Widths of input variables of LUTs, see note [LUT Packing Strategy]
----------------------------------------------------------------------}

-- | Input variables bitwidth. NB: densely-packed!
inVarsBitWidth :: (Functor m, Monad m, MonadIO m) => VarUsePkg -> m Integer
inVarsBitWidth pkg = invars_bitwidth pkg (vu_invars pkg)

inVarBitWidth :: (Functor m, Monad m, MonadIO m) => VarUsePkg -> EId -> m Integer
inVarBitWidth pkg v = invar_bitwidth pkg v

invars_bitwidth :: (Functor m, MonadIO m, Monad m) => VarUsePkg -> [EId] -> m Integer
invars_bitwidth pkg vartys = do 
  sizes <- mapM (invar_bitwidth pkg) vartys
  -- liftIO $ mapM (putStrLn . show) (zip vartys sizes)
  return (sum sizes)

invar_bitwidth :: (Functor m, Monad m, MonadIO m) => VarUsePkg -> EId -> m Integer
invar_bitwidth pkg v
   -- | Integers in known range
  | Just (RInt (IRng l h)) <- neLookup v $ vu_ranges pkg
  = do {- liftIO $ putStrLn ("Found range for variable: " ++ show v)
          liftIO $ putStrLn ("Low:  " ++ show l)
          liftIO $ putStrLn ("High: " ++ show h) -}
       return (intLog2 (max (abs l) (abs h)))

  -- | Array variables in known input range
  | Just (_,w) <- inArrSliceBitWidth pkg v
  = return w
  -- ^ NB: at the moment input struct variables are considered as inputs
  -- ^ in their entirety. Perhaps later on we would like to extend the
  -- ^ range analysis and be more precise and efficient about this.

  -- | Everything else
  | otherwise 
  = tyBitWidth (nameTyp v)


intLog2 :: Integer -> Integer
intLog2 ix = ceiling (logBase 2 dx + 1)
  where dx :: Double
        dx = fromIntegral ix

-- | Input array slice? 
inArrSlice :: VarUsePkg -> EId -> Maybe (Integer,Integer)
inArrSlice pkg x = do 
  RArr rin _ <- neLookup x $ vu_ranges pkg
  case rin of 
    IRng lidx hidx -> return (lidx, hidx)
    _              -> Nothing


inArrSliceBitWidth :: VarUsePkg -> EId -> Maybe (Integer,Integer)
-- | Returns the start, and length positions of the slice, in bits.
inArrSliceBitWidth vpkg x = do
  basety <- isArrayTy_maybe (nameTyp x)
  bwidth <- tyBitWidth basety
  (lidx,hidx) <- inArrSlice vpkg x 
  return $ (bwidth * lidx, bwidth * (hidx - lidx + 1))
  
