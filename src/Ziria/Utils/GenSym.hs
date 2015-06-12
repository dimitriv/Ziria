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
{-# OPTIONS_GHC -Wall #-}
module Ziria.Utils.GenSym (Sym, initGenSym, genSymStr, getSym, setSym) where

import Control.Applicative
import Data.IORef

newtype Sym = Sym (IORef (Int, String))

instance Show Sym where
  show _ = "<<sym>>"

initGenSym :: String -> IO Sym
initGenSym module_name = Sym <$> newIORef (0, module_name)

getSym :: Sym -> IO (Int, String)
getSym (Sym r) = readIORef r

setSym :: Sym -> (Int, String) -> IO ()
setSym (Sym r) n = modifyIORef' r (\_ -> n)

genSymStr :: Sym -> IO String
genSymStr (Sym r) = do
    modifyIORef' r (\(c,n) -> (c+1,n))
    (v,x) <- readIORef r
    return $ x ++ show v
