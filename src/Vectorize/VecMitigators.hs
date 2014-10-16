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
module VecMitigators where

import AstExpr
import AstComp
import AstFreeMonad
import AstUnlabelled ( vint )

import PpComp
import Outputable
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Monad.State


import Opts
import Utils

import qualified TcMonad as TcM 
import TcErrors (ErrCtx(InternalTypeChecking))

import Debug.Trace


mitigateUp :: Maybe SourcePos 
           -> String 
           -> Ty -> Int -> Int -> FreeComp ()
mitigateUp loc _orig ty lo hi
  = let ya_ty = TArray (Literal hi) ty
        xa_ty = if lo == 1 then ty else TArray (Literal lo) ty
        bnd   = vint (hi `div` lo)
        vlo   = vint lo
    in do { ya <- fLetERef (Right ya_ty)
          ; fRepeat Nothing $ 
            fTimes AutoUnroll (vint 0) bnd $ \i -> 
                 do { x <- fBind (fTake1 `returning` xa_ty)
                    ; fReturn ForceInline $ 
                      if lo == 1 
                      then ya .! i .:= x
                      else ya .! (i .* vlo, lo) .:= x
                               -- yikes I have to use both vlo and lo
                    } 
          }

mitigateDn :: Maybe SourcePos
           -> String
           -> Ty -> Int -> Int -> FreeComp ()
mitigateDn loc _orig ty hi lo
  = let xa_ty = TArray (Literal hi) ty
        bnd   = vint $ hi `div` lo     -- yikes I have to do vint
        vlo   = vint $ lo              -- yikes I have to do vint
    in  do { x <- fBind (fTake1 `returning` xa_ty)
           ; if lo == 1 
             then fEmits x
             else fTimes AutoUnroll (vint 0) bnd $ \i ->
                  fEmit (x .! (i .* vlo, lo))
           }                   -- yikes I have to use both vlo and lo


compileMitigs :: GS.Sym -> Comp -> IO Comp
-- Compile away mitigators.
-- TODO: at the moment this is a bit inefficient,
-- better to compile directly in terms of tick and process.
compileMitigs sym comp
  = mapCompM return return return return return compile comp
  where 
    compile c
      | MkComp c0 loc _ <- c
      , Mitigate ty i1 i2 <- c0
      = tcSplice sym $ 
        if i1 <= i2
        then mitigateUp loc "compiled" ty i1 i2
        else mitigateDn loc "compiled" ty i1 i2
      | otherwise
      = return c

-- This should be moved to the TcMonad or the linter
tcSplice :: GS.Sym -> FreeComp () -> IO Comp
tcSplice sym fcomp
  = do { mc <- TcM.runTcM (unFreeComp Nothing fcomp)
                          (TcM.mkTyDefEnv []) 
                          (TcM.mkEnv [])
                          (TcM.mkCEnv []) 
                          sym 
                          InternalTypeChecking 
                          TcM.emptyTcMState
       ; case mc of
           Left err      -> panic err
           Right (c', _) -> return c'
       }

