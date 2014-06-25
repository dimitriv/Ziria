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
import Text.PrettyPrint.Mainland
import Data.Maybe


callInBufInitializer  buf_context heap_context = callExtBufInitializer "get" buf_context heap_context
callOutBufInitializer buf_context heap_context = callExtBufInitializer "put" buf_context heap_context

callExtBufInitializer str buf_context heap_context (ExtBuf base_ty) = 
  let init_typ_spec = "init_" ++ str ++ (fst $ getTyPutGetInfo base_ty)
  in [cstm| $id:(init_typ_spec)($id:buf_context, $id:heap_context);|]

callExtBufInitializer _str _buf_context _heap_context (IntBuf _) 
  = error "BUG: callExtBufInitializer called with IntBuf!" 

cgExtBufInitsAndFins (TBuff in_bty,TBuff out_bty) mfreshId
  = do buf_context  <- getBufContext
       heap_context <- getHeapContext
       appendTopDef [cedecl|void $id:(ini_name mfreshId)() {
                        $stm:(callInBufInitializer buf_context heap_context in_bty)
                        $stm:(callOutBufInitializer buf_context heap_context out_bty)
                        } |]
       appendTopDef [cedecl|void $id:(fin_name mfreshId)() {
                        $stm:(callOutBufFinalizer buf_context out_bty)
                        } |]
  where ini_name mfreshId = "wpl_input_initialize" ++ mfreshId
        fin_name mfreshId = "wpl_output_finalize" ++ mfreshId

cgExtBufInitsAndFins (ty1,ty2) mfreshId
  = fail $ "BUG: cgExtBufInitsAndFins called with non-TBuff types!"

callOutBufFinalizer buf_context (ExtBuf base_ty) =
  let finalize_typ_spec = "flush_put" ++ (fst $ getTyPutGetInfo base_ty)
  in [cstm| $id:(finalize_typ_spec)($id:buf_context);|]
callOutBufFinalizer _buf_context (IntBuf _) 
  = error "BUG: callOutBufFinalizer called with IntBuf!" 

   
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

        buf_context  <- getBufContext
        heap_context <- getHeapContext

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
          loop_counter++;
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
