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
module GenSym (Sym, initGenSym, genSym, getSym, setSym) where

import Data.IORef

type Sym = IORef Int

initGenSym :: IO Sym
initGenSym = newIORef 0

nextSym r = modifyIORef' r (+1)

getSym r = readIORef r
setSym r n = modifyIORef' r (\_ -> n)

genSym :: Sym -> IO Int
genSym r =
  do nextSym r
     readIORef r

