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

module Eval where

import AstExpr
import AstComp

import Text.PrettyPrint.HughesPJ

import PpExpr
import PpComp
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map


import CgLUT ( shouldLUT )
import Analysis.Range ( varRanges )
import Analysis.UseDef ( inOutVars )

import Opts

import Data.Array.IO

import qualified GenSym as GS

import TcExpr ( tyOfParams )

evalArith :: GExp ty a -> Maybe Val
-- Postcondition: if this function returns (Just v) then 
-- v is arithmetic. 
evalArith e = go (unExp e)
  where go (EVal val) = return val
        go (EBinOp b e1 e2)
          = do { v1 <- evalArith e1
               ; v2 <- evalArith e2
               ; val_binop b v1 v2
               }
        go (EUnOp (Cast ty) e1)
          = do { v1 <- evalArith e1
               ; val_cast ty v1
               }
        go (EUnOp u e1)
          = do { v1 <- evalArith e1
               ; val_unop u v1
               }

        go (EArrRead arr ei LISingleton)
         | EValArr vals <- unExp arr
         = do { i <- evalInt ei
              ; case (vals !! fromIntegral i) of 
                  v@(VInt _) -> return v
                  _          -> Nothing 
              }

        go _other = Nothing

val_binop Add  (VInt i1) (VInt i2) = return (VInt $ i1+i2)
val_binop Mult (VInt i1) (VInt i2) = return (VInt $ i1*i2)
val_binop Sub  (VInt i1) (VInt i2) = return (VInt $ i1-i2)
val_binop Div  (VInt i1) (VInt i2) = return (VInt $ i1 `quot` i2)
val_binop Rem  (VInt i1) (VInt i2) = return (VInt $ i1 `rem` i2)

val_binop Add  (VDouble i1) (VDouble i2) = return (VDouble $ i1+i2)
val_binop Mult (VDouble i1) (VDouble i2) = return (VDouble $ i1*i2)
val_binop Sub  (VDouble i1) (VDouble i2) = return (VDouble $ i1-i2)
val_binop Div  (VDouble i1) (VDouble i2) = return (VDouble $ i1 / i2)

val_binop _ _ _ = Nothing

val_unop Neg (VInt i) = return (VInt (-i))
val_unop _ _ = Nothing

val_cast (TInt {})    (VDouble d)    = return (VInt (floor d))
val_cast (TInt {})    (VInt i)       = return (VInt i)
val_cast (TDouble)    (VInt d)       = return (VDouble (fromIntegral d))
val_cast (TInt {})    (VBit False)   = return (VInt 0)
val_cast (TInt {})    (VBit True)    = return (VInt 1)

val_cast _ _ = Nothing



evalInt :: GExp ty a -> Maybe Integer
evalInt e = case evalArith e of
              Just (VInt i) -> return i
              _otherwise    -> Nothing

-- Array initialization code or the form
-- ELetRef nm _ (ESeq init_loop nm)
--
evalArrInt e = evalArrInt0 (unExp e)
evalArrInt0 (ELetRef nm ty Nothing e)
  | TArr (Literal n) (TInt _) <- ty
  , ESeq e1 e2 <- unExp e
  , EVar nm' <- unExp e2
  , nm' == nm
  = do { arr <- newArray (0,n-1) 0 :: IO (IOArray Int Integer)
       ; try_body <- evalArrInitLoop (nm,arr) e1
       ; case try_body of
           Nothing -> return Nothing
           Just () -> do { contents <- getElems arr
                         ; return (Just contents) }
       }
evalArrInt0 other = return Nothing

evalArrInitLoop :: (Name, IOArray Int Integer) -> Exp Ty -> IO (Maybe ())
evalArrInitLoop (nm,arr) exp
  | EFor _ k elow esize ebody <- unExp exp
  , EVal (VInt low) <- unExp elow
  , EVal (VInt siz) <- unExp esize
  , EArrWrite earr eind LISingleton eval <- unExp ebody
  , EVar arr_nm <- unExp earr
  , arr_nm == nm
  , EVar ind_nm <- unExp eind
  , ind_nm == k
  , let inds = [fromIntegral low..(fromIntegral low + fromIntegral siz - 1)] -- Indices
  , let loc = expLoc exp
  = let subst_idx i = substExp (k, MkExp (EVal (VInt i)) loc tint) eval
    in case mapM (\i -> subst_idx i >>= evalInt) inds of
         Nothing -> return Nothing
         Just vals ->
           do { mapM (\(val,i) -> writeArray arr (fromIntegral i) val) (zip vals inds)
              ; return (Just ()) }

evalArrInitLoop nm exp
  = return Nothing



evalBool :: GExp ty a -> Maybe Bool
-- A quite incomplete boolean evaluator
evalBool e 
  | (EBinOp Eq e1 e2) <- unExp e
  , Just i1 <- evalInt e1
  , Just i2 <- evalInt e2
  = Just (i1 == i2)
  | (EVal (VBool b)) <- unExp e
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


  

