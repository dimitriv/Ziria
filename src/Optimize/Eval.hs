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
{-# OPTIONS_GHC -Wall #-}
module Eval (evalArith, evalInt, evalArrInt, evalBool) where

import Control.Monad (forM)
import Data.Array

import AstExpr

{-------------------------------------------------------------------------------
  Compile time casting of expressions

  This we may call this at various points in the compiler, we can cast using
  both source types and internall types.
-------------------------------------------------------------------------------}

class TypeAnn ty where
  valCast :: ty -> Val -> Maybe Val

instance TypeAnn Ty where
  valCast (TInt {}) (VDouble d)  = return (VInt (floor d))
  valCast (TInt {}) (VInt i)     = return (VInt i)
  valCast (TDouble) (VInt d)     = return (VDouble (fromIntegral d))
  valCast (TInt {}) (VBit False) = return (VInt 0)
  valCast (TInt {}) (VBit True)  = return (VInt 1)
  valCast _         _            = Nothing

instance TypeAnn (Maybe SrcTy) where
  valCast (Just (SrcTInt {})) (VDouble d)  = return (VInt (floor d))
  valCast (Just (SrcTInt {})) (VInt i)     = return (VInt i)
  valCast (Just (SrcTDouble)) (VInt d)     = return (VDouble (fromIntegral d))
  valCast (Just (SrcTInt {})) (VBit False) = return (VInt 0)
  valCast (Just (SrcTInt {})) (VBit True)  = return (VInt 1)
  valCast _            _            = Nothing

{-------------------------------------------------------------------------------
  Simple static evaluation of expressions
-------------------------------------------------------------------------------}

evalArith :: TypeAnn ty => GExp ty a -> Maybe Val
evalArith e = go (unExp e)
  where
    go (EVal _ val)         = return val
    go (EBinOp b e1 e2)     = do v1 <- evalArith e1
                                 v2 <- evalArith e2
                                 valBinOp b v1 v2
    go (EUnOp (Cast ty) e1) = do v1 <- evalArith e1
                                 valCast ty v1
    go (EUnOp u e1)         = do v1 <- evalArith e1
                                 valUnOp u v1

    go (EArrRead arr ei LISingleton)
     | EValArr _t vals <- unExp arr
     = do i <- evalInt ei
          case (vals !! fromIntegral i) of 
            v@(VInt _) -> return v
            _          -> Nothing

    go _other               = Nothing

evalInt :: TypeAnn ty => GExp ty a -> Maybe Integer
evalInt e = case evalArith e of
              Just (VInt i) -> return i
              _otherwise    -> Nothing

valBinOp :: BinOp -> Val -> Val -> Maybe Val
valBinOp Add  (VInt i1) (VInt i2) = return (VInt $ i1+i2)
valBinOp Mult (VInt i1) (VInt i2) = return (VInt $ i1*i2)
valBinOp Sub  (VInt i1) (VInt i2) = return (VInt $ i1-i2)
valBinOp Div  (VInt i1) (VInt i2) = return (VInt $ i1 `quot` i2)
valBinOp Rem  (VInt i1) (VInt i2) = return (VInt $ i1 `rem` i2)

valBinOp Add  (VDouble i1) (VDouble i2) = return (VDouble $ i1+i2)
valBinOp Mult (VDouble i1) (VDouble i2) = return (VDouble $ i1*i2)
valBinOp Sub  (VDouble i1) (VDouble i2) = return (VDouble $ i1-i2)
valBinOp Div  (VDouble i1) (VDouble i2) = return (VDouble $ i1 / i2)

valBinOp _ _ _ = Nothing

valUnOp :: GUnOp ty -> Val -> Maybe Val
valUnOp Neg (VInt i) = return (VInt (-i))
valUnOp _ _ = Nothing

{-------------------------------------------------------------------------------
  Static execution of array initialization code

  NOTE: We used to support more kinds of initialization code, but that was
  commented out and badly bitrotted. Search history for `evalArrInit`.
-------------------------------------------------------------------------------}

-- | Execute array initialization code of the form
-- > ELetRef nm Nothing (ESeq init_loop nm)

evalArrInt :: Exp -> Maybe [Integer]
evalArrInt = evalArrInt0 . unExp

evalArrInt0 :: Exp0 -> Maybe [Integer]
evalArrInt0 (ELetRef nm Nothing e)
  | Just n     <- intArrayLen (nameTyp nm)
  , ESeq e1 e2 <- unExp e
  , EVar nm'   <- unExp e2
  , nm' == nm
  = do values <- evalArrInitLoop nm e1
       return $ elems $ array (0, n - 1) values
evalArrInt0 _other = Nothing


-- | Execute an array initialization loop of the form
--
-- > for k in [low, siz] { arr[k] := eval }
evalArrInitLoop :: GName Ty -> Exp -> Maybe [(Int, Integer)]
evalArrInitLoop arr loop
  | EFor _ k elow esize ebody            <- unExp loop
  , EVal _ (VInt low)                    <- unExp elow
  , EVal _ (VInt siz)                    <- unExp esize
  , EArrWrite earr eind LISingleton eval <- unExp ebody
  , EVar arr'                            <- unExp earr
  , EVar k'                              <- unExp eind
  , arr' == arr
  , k'   == k
  = let inds = [fromIntegral low .. fromIntegral low + fromIntegral siz - 1]
        loc  = expLoc loop
        subst_idx i = substExp [] 
                         [(k, MkExp (EVal (nameTyp k) (VInt i)) loc ())] eval
    in forM inds $ \i -> do
         v <- evalInt $ subst_idx i
         return (fromInteger i, v)

evalArrInitLoop _ _
  = Nothing

intArrayLen :: Ty -> Maybe Int
intArrayLen (TArray (Literal n) (TInt _)) = Just n
intArrayLen _ = Nothing


evalBool :: TypeAnn ty => GExp ty a -> Maybe Bool
-- A quite incomplete boolean evaluator
evalBool e 
  | (EBinOp Eq e1 e2) <- unExp e
  , Just i1 <- evalInt e1
  , Just i2 <- evalInt e2
  = Just (i1 == i2)
  | (EVal _ (VBool b)) <- unExp e
  = Just b
  | (EUnOp Neg e1) <- unExp e
  , Just b <- evalBool e1
  = Just (not b)
  | (EBinOp And e1 e2) <- unExp e
  , Just b1 <- evalBool e1
  , Just b2 <- evalBool e2
  = Just (b1 && b2)
  | otherwise
  = Nothing 
