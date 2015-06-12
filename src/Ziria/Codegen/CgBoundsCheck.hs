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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Ziria.Codegen.CgBoundsCheck ( cgBoundsCheck ) where

import Control.Monad ( unless )
import Data.Loc
import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Ziria.BasicTypes.AstExpr
import Ziria.Codegen.CgMonad
import Ziria.Utils.Utils

import Opts

{-------------------------------------------------------------------------------
  Bounds checking
-------------------------------------------------------------------------------}

cgBoundsCheck :: DynFlags
              -> SrcLoc
              -> Ty          -- ^ Array type 
              -> C.Exp       -- ^ Start index
              -> LengthInfo  -- ^ Range to check
              -> Cg ()
cgBoundsCheck dflags loc arrty cbase linfo
   | isDynFlagSet dflags BoundsCheck
   , TArray numexpr _ <- arrty
   = do is_disabled <- isDisabledBC
        unless is_disabled $
          appendStmt [cstm|bounds_check(
                              $(cnumexpr numexpr),
                              $cbase + $int:(leninfo linfo),
                              $string:spos); 
                     |]
   | otherwise = return ()
   where 
     leninfo LISingleton  = 0
     leninfo (LILength n) = n-1
     leninfo (LIMeta {})  = panicStr "cgBoundsCheck: LIMeta!"
     cnumexpr (Literal m) = [cexp| $int:m|]
     cnumexpr (NVar nm)   = [cexp| $id:nm|]
     spos = displayLoc (locOf loc)
