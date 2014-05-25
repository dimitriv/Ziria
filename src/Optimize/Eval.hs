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




evalInt :: Exp a -> Maybe Int 
-- An arithmetic evaluator
evalInt e = evalInt0 (unExp e)
evalInt0 (EVal (VInt i)) = return i
evalInt0 (EBinOp Add e1 e2)
  = do { i1 <- evalInt e1
       ; i2 <- evalInt e2
       ; return (i1+i2) }
evalInt0 (EBinOp Sub e1 e2)
  = do { i1 <- evalInt e1
       ; i2 <- evalInt e2
       ; return (i1-i2) }
evalInt0 (EBinOp Mult e1 e2)
  = do { i1 <- evalInt e1
       ; i2 <- evalInt e2
       ; return (i1*i2) }
evalInt0 (EBinOp Div e1 e2)
  = do { i1 <- evalInt e1
       ; i2 <- evalInt e2
       ; return (i1 `quot` i2) }
evalInt0 (EBinOp Rem e1 e2) 
  = do { i1 <- evalInt e1
       ; i2 <- evalInt e2
       ; return (i1 `rem` i2) }
evalInt0 _ = Nothing


-- Array initialization code or the form
-- ELetRef nm _ (ESeq init_loop nm) 
-- 
evalArrInt e = evalArrInt0 (unExp e)
evalArrInt0 (ELetRef nm nfo e)
  | Left (TArr (Literal n) (TInt _)) <- nfo
  , ESeq e1 e2 <- unExp e
  , EVar nm' <- unExp e2
  , nm' == nm 
  = do { arr <- newArray (0,n-1) 0 :: IO (IOArray Int Int)
       ; try_body <- evalArrInitLoop (nm,arr) e1
       ; case try_body of 
           Nothing -> return Nothing
           Just () -> do { contents <- getElems arr
                         ; return (Just contents) } 
       }
evalArrInt0 other = return Nothing

evalArrInitLoop :: (Name, IOArray Int Int) -> Exp Ty -> IO (Maybe ())
evalArrInitLoop (nm,arr) exp 
  | EFor k elow esize ebody <- unExp exp
  , EVal (VInt low) <- unExp elow
  , EVal (VInt siz) <- unExp esize
  , EArrWrite earr eind LISingleton eval <- unExp ebody
  , EVar arr_nm <- unExp earr
  , arr_nm == nm
  , EVar ind_nm <- unExp eind
  , ind_nm == k 
  , let inds = [low..(low+siz-1)] -- Indices
  , let loc = expLoc exp
  = let subst_idx i = substExp (k, MkExp (EVal (VInt i)) loc tint) eval
    in case mapM (\i -> subst_idx i >>= evalInt) inds of
         Nothing -> return Nothing 
         Just vals -> 
           do { mapM (\(val,i) -> writeArray arr i val) (zip vals inds)
              ; return (Just ()) }

evalArrInitLoop nm exp
  = return Nothing

-- case mapM (\i -> substExp (k, MkExp (EVal (VInt i))  eval) inds of
--       Nothing   -> return Nothing -- One of the values could not evaluate
--       Just vals -> do { mapM (\(val,i) -> writeArray arr i val) (zip vals inds)
--                       ; return (Just ()) }

--   | otherwise 
--   = return Nothing

{- 

let inds = [low:low+siz-1] -- Indices
    in 
    case mapM (\i -> substAll (k,EVal (VInt i)) eval) inds of
      Nothing   -> return Nothing -- One of the values could not evaluate
      Just vals -> do { mapM (\(val,i) -> writeArray arr i val) (zip vals inds)
                      ; return (Just ()) }
-}

-- evalArrInit :: [Name,IORef [Int]] -> Exp Ty -> IO (Maybe [Int])
-- -- Evaluate array initialization loops statically
-- evalArrInit env exp = evalArrInit0 env (unExp exp)
-- evalArrInit0 (Var nm)
--   | Just ref <- lookup env nm
--   = do { contents <- readIORef ref
--        ; return $ Just contents }
--   | Nothing
--   = return Nothing

-- evalArrInit (ESeq e1 e2)
--   = do { evalUnit e1
--        ; evalArrInt e2 }

-- evalArrInt0 (ELetRef nm nfo e)
--   | Left (TArr _ TInt) <- nfo
--   = do { arr <- newIORef Int 0
--        ; evalArrUnit ((nm,arref):env) e }
--   | otherwise
--   = return Nothing

-- evalArrUnit (EIter nm lower size e)
--   | EVal (VInt l) <- unExp lower
--   , EVal (VInt s) <- unExp size
--   = let es = replicate s e
--         is = [l..(l+s-1)]
--         substs = zip (repeat nm) is
--         es' = zipWith (\(s,e) -> substAll s e) subst es
--     in mapM evalUnit es'
-- evalArrUnit env (EArrWrite earr eind eval)
--   | (EVar nm) <- unExp earr
--   , Just arref <- lookup nm env
--   , (EVal (VInt i)) <- eind
--   , Just r <- evalInt eval
--   = do { writeIORef ........... }


-- evalArrUnit _ = return Nothing

