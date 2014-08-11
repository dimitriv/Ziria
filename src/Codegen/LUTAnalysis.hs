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

module LUTAnalysis ( LUTStats, calcLUTStats, pprLUTStats

                   , shouldLUT, lutTableSize

                   , varBitWidth, varsBitWidth, varsBitWidth_ByteAlign

                   , expResultVar 
                   ) where              

import Opts
import AstExpr
import CgTypes
import Analysis.Range
import Analysis.UseDef

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.State  (MonadState(..), modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Text.PrettyPrint.Mainland


varsBitWidth_ByteAlign :: (Functor m, Monad m) => [VarTy] -> m Int
varsBitWidth_ByteAlign vartys 
  = do { sizes <- mapM (tyBitWidth_ByteAlign . snd) vartys
       ; return $ sum sizes 
       }

varsBitWidth :: (Functor m, Monad m) => Map Name Range -> [VarTy] -> m Int
varsBitWidth ranges vartys 
  = do { sizes <- mapM (\(v,ty) -> varBitWidth ranges v ty) vartys 
       ; return (sum sizes) }
varBitWidth :: Monad m => Map Name Range -> Name -> Ty -> m Int
varBitWidth ranges v ty | Just (Range _ h) <- Map.lookup v ranges =
    return $ intLog2 h
  where
    intLog2 :: Integer -> Int
    intLog2 = ceiling . (+1) . logBase 2 . fromIntegral
varBitWidth _ _ ty = tyBitWidth ty


-- If the given expression's value is a variable 'v', then return 'Just v',
-- otherwise return 'Nothing'.
expResultVar :: Exp Ty -> Maybe Name
expResultVar (MkExp (ESeq _ e2) _ _) = expResultVar e2
expResultVar (MkExp (EVar v) _ ty)   = Just v
expResultVar _                       = Nothing

data LUTStats = LUTStats { lutInBitWidth      :: Int
                         , lutOutBitWidth     :: Int
                         , lutInVarsBitWidth  :: Int
                         , lutOutVarsBitWidth :: Int
                         , lutResultBitWidth  :: Int
                         , lutResultInOutVars :: Bool
                         , lutTableSize       :: Integer
                         }

instance Pretty LUTStats where
    ppr s = 
        text "   result bitsize:" <+> (if lutResultInOutVars s
                                       then text "included in output variables"
                                       else ppr (lutResultBitWidth s)) </>
        text "    input bitsize:" <+> ppr (lutInBitWidth s) </>
        text "   output bitsize:" <+> ppr (lutOutBitWidth s) </>
        text "lut size in bytes:" <+> ppr (lutTableSize s)

calcLUTStats :: (Functor m, Monad m)
             => [(Name,Ty)]
             -> Map Name Range
             -> Exp Ty
             -> m LUTStats
calcLUTStats locals ranges e = do
    (inVars, outVars, _) <- inOutVars locals ranges e
    inVarsBitWidth       <- varsBitWidth ranges inVars
    outVarsBitWidth      <- varsBitWidth ranges outVars
    let resultInOutVars  =  case expResultVar e of
                              Just v | v `elem` map fst outVars -> True
                              _ -> False 
    resultBitWidth       <- if resultInOutVars
                            then return 0
                            else tyBitWidth (info e)
    let inBitWidth       =  inVarsBitWidth
    let outBitWidth      =  outVarsBitWidth + resultBitWidth
    return LUTStats { lutInBitWidth      = inBitWidth
                    , lutOutBitWidth     = outBitWidth
                    , lutInVarsBitWidth  = inVarsBitWidth
                    , lutOutVarsBitWidth = outVarsBitWidth
                    , lutResultBitWidth  = resultBitWidth
                    , lutResultInOutVars = resultInOutVars
                    , lutTableSize = calcLutSizeInBytes inBitWidth outBitWidth
                    }
  where
    calcLutSizeInBytes :: Int     -- ^ Input width in bits
                       -> Int     -- ^ Output width in bits
                       -> Integer -- ^ Table size in bytes
    calcLutSizeInBytes inw outw =
      -- num entries * bytes per entry
      2^(fromIntegral inw :: Integer) * ((fromIntegral outw + 7) `div` 8)

pprLUTStats :: Monad m
            => DynFlags
            -> [(Name,Ty)]
            -> Map Name Range
            -> Exp Ty 
            -> m Doc
pprLUTStats dflags locals ranges e = do
    (inVars, outVars, allVars) <- inOutVars locals ranges e
    return $ text "  input variables:" <+> ppr inVars </>
             text " output variables:" <+> ppr outVars </>
             text "    all variables:" <+> ppr allVars </>
             text "      locals used:" <+> ppr locals </>
             text "      ranges used:" <+> ppr ranges <>
             case calcLUTStats locals ranges e of
               Nothing    -> mempty
               Just stats -> line <>
                             ppr stats </>
                             text " should be lutted:" <+> pprShouldLUT (shouldLUT dflags locals ranges e)
  where
    pprShouldLUT :: Either String Bool -> Doc
    pprShouldLUT (Left err)    = text "No, because" <+> string err
    pprShouldLUT (Right False) = text "No"
    pprShouldLUT (Right True)  = text "Yes"

newtype LM a = LM { runLM :: LMState -> Either String (a, LMState) }

evalLM :: LM a -> LMState -> Either String a
evalLM m s = case runLM m s of
               Left  err    -> Left err
               Right (a, _) -> Right a

data LMState = LMState { lmHasLoop  :: !Bool
                       , lmHasCall  :: !Bool
                       , lmHasLocalFunction :: !Bool
                       , lmHasLUT   :: !Bool
                       , lmOpCount  :: !Int
                       , lmHasBPerm :: !Bool
                       }

instance Monad LM where
    return a = LM $ \s -> Right (a, s)
    m1 >> m2 = LM $ \s -> case runLM m1 s of
                                Left  err     -> Left err
                                Right (_, s') -> runLM m2 s'
    m1 >>= k = LM $ \s -> case runLM m1 s of
                                Left  err     -> Left err
                                Right (a, s') -> runLM (k a) s'
    fail err = LM $ \_ -> Left err

instance Functor LM where
    fmap f x = x >>= return . f

instance Applicative LM where
    pure   = return
    (<*>)  = ap

instance MonadState LMState LM where
    get   = LM $ \s -> Right (s,  s)
    put s = LM $ \_ -> Right ((), s)


mIN_OP_COUNT :: Int
mIN_OP_COUNT = 5

shouldLUT :: DynFlags -> [(Name,Ty)] -> Map Name Range -> Exp Ty -> Either String Bool
shouldLUT dflags locals ranges e = flip evalLM s0 $ do
    stats <- calcLUTStats locals ranges e
    should e
    s <- get
    
    

    return $ and [ not (lmHasCall s)
                 , not (lmHasLUT s)
                 , not (lmHasBPerm s) -- Bit permutation will independently be compiled to LUTs
                                      -- so this would amount to a lut within a lut
                 , lutOutBitWidth stats /= 0 && (lutOutBitWidth stats > 2) 
                   -- LUTting only if bitwidth of output is > 2 bits, otherwise probably it's not worth it
                 , lmOpCount s > mIN_OP_COUNT
                 , lutTableSize stats <= maxLUTSize dflags
                 ]
  where
    s0 :: LMState
    s0 = LMState { lmHasLoop = False
                 , lmHasCall = False
                 , lmHasLocalFunction = False
                 , lmHasLUT  = False
                 , lmOpCount = 0
                 , lmHasBPerm = False
                 }

    should :: Exp Ty -> LM ()
    should (MkExp e _ _) =
        go e

    go :: Exp0 Ty -> LM ()
    go e@(EVal {})    = return ()
    go e@(EValArr {}) = return ()
    go e@(EVar {})    = return ()

    go (EUnOp _ e) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e

    go (EBinOp _ e1 e2) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e1 >> should e2

    -- go (EComplex e1 e2) =
    --     should e1 >> should e2

    go (EAssign e1 e2) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e1 >> should e2

    go (EArrRead e1 e2 _) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e1 >> should e2

    go (EArrWrite e1 e2 _ e3) = do
        modify $ \s -> s { lmOpCount = lmOpCount s + 1 }
        should e1 >> should e2 >> should e3

    go (EIter _ _ e1 e2) = do
        modify $ \s -> s { lmHasLoop = True }
        should e1 >> should e2
        -- making sure that we LUT loops more often
        modify $ \s -> s { lmOpCount = max (lmOpCount s) (mIN_OP_COUNT+1) }

    go (EFor _ _ e1 e2 e3) = do
        modify $ \s -> s { lmHasLoop = True }
        should e1 >> should e2 >> should e3
        -- making sure that we LUT loops more often
        modify $ \s -> s { lmOpCount = max (lmOpCount s) (mIN_OP_COUNT+1) }

    go (EWhile e1 e2) = do
        modify $ \s -> s { lmHasLoop = True }
        should e1 >> should e2
        -- making sure that we LUT loops more often
        modify $ \s -> s { lmOpCount = max (lmOpCount s) (mIN_OP_COUNT+1) }

    

    go (ELet _ e1 e2) =
        should e1 >> should e2

    go (ELetRef _ (Left {}) e2) = 
        should e2
    go (ELetRef _ (Right e1) e2) = 
        should e1 >> should e2

    go (ESeq e1 e2) =
        should e1 >> should e2

    go (ECall _ es) = do
        modify $ \s -> s { lmHasCall = True }
        mapM_ should es


    go (EIf e1 e2 e3) =
        should e1 >> should e2 >> should e3

    go (EPrint _ e) =
        should e

    go e@(EError {})    = return ()

    go (ELUT _ e) = do
        modify $ \s -> s { lmHasLUT = True }
        should e

    go (EBPerm e1 e2) = do
       modify $ \s -> s { lmHasBPerm = True }
       should e1 >> should e2

    go (EStruct tn tfs) 
       = mapM_ (\(fn,fe) -> should fe) tfs

    go (EProj e fn)     
       = should e
