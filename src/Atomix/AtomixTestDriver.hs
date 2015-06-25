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
{-# OPTIONS -Wall #-}

module Main where

--
import AtomComp
import AutomataModel
import AtomInstantiation

atomixTest :: CompM () (Automaton FunLikeAtom Int)
atomixTest = do
  wifi <- mkWifi
  inch  <- freshVar "src" undefined Mut
  outch <- freshVar "snk" undefined Mut
  let channels = Channels { in_chan = inch, out_chan = outch, ctrl_chan = Nothing }
      k = mkDoneAutomaton inch outch
  mkAutomaton undefined channels wifi k

main :: IO ()
main = do 
  (a,_) <- runCompM atomixTest
  putStrLn $ show a
  return ()
