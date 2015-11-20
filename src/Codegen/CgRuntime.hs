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

module CgRuntime where

import Opts
import AstExpr
import AstComp
import PpComp
import qualified GenSym as GS
import CgHeader
import CgMonad
import CgTypes

import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ 
import Data.Maybe



callInBufInitializer  buf_context heap_context 
  = callExtBufInitializer "get" buf_context heap_context
callOutBufInitializer buf_context heap_context 
  = callExtBufInitializer "put" buf_context heap_context

callExtBufInitializer str global_params 
                          buf_context 
                          heap_context (ExtBuf base_ty) 
  = let atom_ty = atomTyOf base_ty
        init_typ_spec = "init_" ++ str ++ (fst $ getTyPutGetInfo atom_ty)
    in [cstm| $id:(init_typ_spec)($id:global_params, 
                                    $id:buf_context, $id:heap_context, $(tySizeOf_C atom_ty));|]

callExtBufInitializer _str _global_params _buf_context _heap_context (IntBuf _) 
  = error "BUG: callExtBufInitializer called with IntBuf!" 

cgExtBufInitsAndFins (TBuff in_bty,TBuff out_bty) mfreshId
  = do buf_context  <- getBufContext
       heap_context <- getHeapContext
       global_params <- getGlobalParams
       fin_stms <- getFinalizerStmts
       appendTopDef [cedecl|void $id:(ini_name mfreshId)() {
                        $stm:(callInBufInitializer global_params 
                                                   buf_context 
                                                   heap_context in_bty)
                        $stm:(callOutBufInitializer global_params 
                                                    buf_context 
                                                    heap_context out_bty)
                        } |]
       appendTopDef [cedecl|void $id:(fin_name mfreshId)() {
                        $stms:fin_stms
                        $stm:(callOutBufFinalizer global_params 
                                                  buf_context out_bty)
                        } |]
       appendTopDef [cedecl|void $id:(reset_name mfreshId)() {
                        $stm:(callOutBufReset global_params buf_context out_bty)
                        } |]
  where ini_name mfreshId = "wpl_input_initialize" ++ mfreshId
        fin_name mfreshId = "wpl_output_finalize" ++ mfreshId
        reset_name mfreshId = "wpl_output_reset" ++ mfreshId

cgExtBufInitsAndFins (ty1,ty2) mfreshId
  = fail $ "BUG: cgExtBufInitsAndFins called with non-TBuff types!"

callOutBufFinalizer global_params buf_context (ExtBuf base_ty) =
  let atom_ty = atomTyOf base_ty 
      finalize_typ_spec = "flush_put" ++ (fst $ getTyPutGetInfo atom_ty)
  in [cstm| $id:(finalize_typ_spec)($id:global_params, $id:buf_context);|]

callOutBufFinalizer _global_params _buf_context (IntBuf _) 
  = error "BUG: callOutBufFinalizer called with IntBuf!" 

callOutBufReset global_params buf_context (ExtBuf base_ty) =
  let atom_ty = atomTyOf base_ty 
      finalize_typ_spec = "reset_put" ++ (fst $ getTyPutGetInfo atom_ty)
  in [cstm| $id:(finalize_typ_spec)($id:global_params, $id:buf_context);|]

callOutBufReset _global_params _buf_context (IntBuf _) 
  = error "BUG: callOutBufReset called with IntBuf!" 


mkRuntime :: Maybe String   -- | Optional unique suffix for name generation
          -> Cg CompInfo    -- | Computation that generates code for a computation
          -> Cg ()
mkRuntime mfreshId m = do
    (local_decls, local_stmts, cinfo) <- inNewBlock m
    go cinfo local_decls local_stmts
    -- go True  cinfo local_decls local_stmts
  where
    default_lbl = "l_DEFAULT_LBL"

    go :: CompInfo -> [C.InitGroup] -> [C.Stm] -> Cg ()
    go cinfo local_decls local_stmts = do
        let (_, init_decls, init_stms) = getCode (compGenInit cinfo)

        appendTopDef $ 
          [cedecl|int $id:(go_name mfreshId ++ "_aux")(int initialized) {
                       unsigned int loop_counter = 0;

                       $decls:init_decls             
                       $decls:local_decls             
                       if (!initialized) {
                         $stms:init_stms
                       }
                       $id:(default_lbl): {
                         $stms:(main_body cinfo local_stmts)
                       }
                  } |]

        appendTopDef $ 
          [cedecl|int $id:(go_name mfreshId)() {
                     return ($id:(go_name mfreshId ++ "_aux")(0));
                  }
          |]

    main_tick cinfo 
      | canTick cinfo 
      = [cstm|goto $id:(tickNmOf (tickHdl cinfo));|]
      | otherwise     
      = [cstm|goto l_CONSUME;|]

    main_body :: CompInfo
              -> [C.Stm]
              -> [C.Stm]
    main_body cinfo stmts = 
       [cstm|
        l_LOOP:
         {
            $stm:(main_tick cinfo)
            l_IMMEDIATE:
              switch($id:globalWhatIs) {
              case SKIP:
                goto l_LOOP;
              case YIELD:
                printf("BUG in code generation: YIELD!"); exit(-1);
              case DONE:
                return 0;
              }
              return 2; // error
            l_CONSUME:
              printf("BUG in code generation: CONSUME!"); exit(-1);
        }|] : stmts ++ [ [cstm| return 2;|] ]

go_name mfreshId = 
  let goName = "wpl_go" 
  in goName ++ fromMaybe "" mfreshId





mkAtomixRuntime :: DynFlags     -- | Flags
                -> Int          -- | number of threads
                -> Int          -- | this thread id
                -> Maybe String -- | Optional unique suffix for name generation
                -> Cg CLabel    -- | Computation that generates code
                -> Cg ()
mkAtomixRuntime dfs no_threads atid mfreshId m = do
    (local_decls, local_stmts, clabel) <- inNewBlock m
    appendTopDef $ [cedecl| extern $ty:(namedCType "bool") atomix; |]
    appendTopDef $ [cedecl|
      int $id:(go_name mfreshId)() {
          
          // Ask driver.c to look for atomix threads
          atomix = 1;

          $decls:local_decls
          
          $stms:init_barr
          goto $id:clabel;

          $stms:local_stmts

          exit(2);
      }
    |]
  where 
    barr_name label = "__barr_" ++ label
    no_threads      = getNoAtomThreads dfs

    init_barr = if (no_threads > 1)  
                   then [[cstm| barrier($int:no_threads, $int:atid);|]]
                   else [] 
