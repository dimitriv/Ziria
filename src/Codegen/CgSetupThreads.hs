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
{-# LANGUAGE  QuasiQuotes #-}

module CgSetupThreads ( thread_setup, thread_setup_shim ) where

import AstExpr
import AstComp
import PpComp
import qualified GenSym as GS
import CgHeader
import CgTypes
import CgMonad
import CgOpt


import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Map as M
import Text.PrettyPrint.Mainland
import Data.Maybe


thread_setup :: Int      -- affinity mask
             -> [Ty]     -- types of internal thread buffers, in order from 0..num_thread_bufs-1
             -> [String] -- list of thread ids
             -> [C.Definition]                
thread_setup affinity_mask buf_tys tids
  = defsPkg
  where my_sizes_decl = [cdecl|typename size_t my_sizes[$int:(num_thread_bufs+1)];|]
        tids'         = tids
        ts_init_stmts = if (num_thread_bufs > 0) 
                        then [ [cstm| ts_init($int:num_thread_bufs,my_sizes); |] ] 
                        else []
        defsPkg  = thread_wrappers
                   ++ [[cedecl| int wpl_set_up_threads(typename PSORA_UTHREAD_PROC * User_Routines) {
                                 typename HANDLE procHdl = GetCurrentProcess();
                                 typename DWORD_PTR affinityMask = $int:affinity_mask;
                                 SetProcessAffinityMask(procHdl, affinityMask);
                                 $decl:my_sizes_decl;
                                 // no_threads = $id:no_threads; 
                                 $stms:(my_sizes_inits (zip [0..] buf_tys)) 
                                 $stms:(user_routines (zip [0..] $ map ("__go" ++) tids'))
                                 $stms:ts_init_stmts
                                 return ($id:no_threads);
                               } 
                      |]]

        no_threads      = show $ length tids'
        num_thread_bufs = length buf_tys
  
        -- initialize User_Routines buffer
        user_routines :: [(Int,String)] -> [C.Stm]
        user_routines = map (\(i,tid) -> [cstm|User_Routines[$id:(show i)] = $id:tid;|])
        -- user_routines []               = []
        -- user_routines ((i,tid) : tids') = 
        --   [cstm|User_Routines[$id:(show i)] = $id:tid;|] : user_routines tids'

        -- initialize my_sizes buffer
        my_sizes_inits = map (\(i,ty) -> [cstm| my_sizes[$int:i] = $(tySizeOf_C ty); |])

        -- my_sizes_inits []             = []
        -- my_sizes_inits ((i,ty) : tys) = 
        --   [cstm| my_sizes[$int:i] = $(tySizeOf_C ty); |] : my_sizes_inits tys

        -- invariant: 2*length (tail tids) == length buf_tys
        -- proof: each thread except main_thread generates two thread comm. buffers
        thread_wrappers = h tids (zip [0..] buf_tys)
        h :: [String] -> [(Int,Ty)] -> [C.Definition]
        h [] [] = []
        h (tid : tids') ((o,oty) : buf_tys') =
          [cedecl| typename BOOLEAN ($id:("__go" ++ tid))(void * pParam) {
	             thread_info *ti; 
                     ti = (typename thread_info *)pParam; 

	             printf("Standalone thread (%s) ID: %u\n", $string:(tid), GetCurrentThreadId());

                     // NB: we must ensure that child threads are never iteration-limited, 
                     // even in debug mode. otherwise, we may get deadlock when debugging
                     // if child and main threads run at different iteration rates.
                     ($id:("wpl_go" ++ tid))();
                     //previously: ($id:bufPutF)($int:(o), $id:(bufput_suffix ++ "EOF"));                     
                     ts_finish($int:o);
 	             (ti->fRunning) = 0;
	             return 0;
                   } 
          |] : h tids' buf_tys'
        h (tid : tids') [] =
          [cedecl| typename BOOLEAN ($id:("__go" ++ tid))(void * pParam) {
	             thread_info *ti; 
                     ti = (typename thread_info *)pParam; 

	             printf("Standalone thread (%s) ID: %u\n", $string:(tid), GetCurrentThreadId());

                     ($id:("wpl_go" ++ tid))();
 	             (ti->fRunning) = 0;
	             return 0;
                   } 
          |] : []
        h _ _ = error "Error: CgSetupThreads: internal code generation error!"



thread_setup_shim :: [C.Definition]
thread_setup_shim 
   = [ [cedecl|extern int SetUpThreads(typename PSORA_UTHREAD_PROC * User_Routines); 
       |]

     , [cedecl|int wpl_set_up_threads(typename PSORA_UTHREAD_PROC * User_Routines) 
               { return (SetUpThreads(User_Routines)); }
       |]
     ]

