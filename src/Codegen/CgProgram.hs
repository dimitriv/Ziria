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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall -Werror #-}

module CgProgram ( codeGenProgram ) where

import Prelude

import Opts
import AstExpr
import AstComp
import CgHeader
import CgRuntime
import CgMonad
import CgTypes
import CgOpt
import CtComp

import qualified CgSetupThreads as ST
import qualified PassPipeline as PP

import Control.Monad.Writer
import Data.Loc
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Data.Maybe

import CgAtomix
import AtomixCompTransform
import AutomataModel
import AtomInstantiation

codeGenContexts :: Cg [C.InitGroup]
-- Declare external context blocks
codeGenContexts
  = do { buf_context  <- getBufContext
       ; heap_context <- getHeapContext
       ; global_params <- getGlobalParams
       ; return [ [cdecl| extern $ty:(namedCType "BufContextBlock")* $id:buf_context;|]
                , [cdecl| extern $ty:(namedCType "HeapContextBlock")* $id:heap_context;|]
                , [cdecl| extern $ty:(namedCType "BlinkParams")* $id:global_params;|]
                , [cdecl| extern $ty:(namedCType "int") stop_program;|]
                ]
       }

codeGenWPLGlobalInit :: [C.Stm] -> String -> Cg ()
codeGenWPLGlobalInit stms mfreshId
  = do { heap_context  <- getHeapContext
       ; appendTopDef $
         [cedecl|void $id:(wpl_global_name) ($ty:(namedCType("memsize_int")) max_heap_siz)
            {
              wpl_init_heap ($id:heap_context, max_heap_siz);
              $stms:stms
            }
         |]
       }
  where wpl_global_name = "wpl_global_init" ++ mfreshId


codeGenCompilerGlobals :: String
                       -> Handle
                       -> Handle
                       -> Maybe Ty
                       -> Ty
                       -> Ty
                       -> Cg ()
codeGenCompilerGlobals tid _tickHdl _procHdl mtv _ta _tb = do
    appendCodeGenDeclGroup (doneValOf $ threadIdOf tid globalDoneHdl)
                           (fromMaybe tint mtv) ZeroOut
    appendDecl [cdecl| char $id:globalWhatIs;|]


codeGenThread :: DynFlags
              -> String  -- thread id
              -> Comp    -- computation (split) to be compiled
                         -- already including the right read/write buffers
              -> Cg ()
codeGenThread dflags tid c
  = do (maybe_tv, ta, tb) <- checkCompType (ctComp c)
       (_bta, _btb) <- checkInOutFiles ta tb
       withThreadId tid $ do
          mkRuntime (Just ((getName dflags) ++ tid)) $ do
            cinfo <- codeGenCompTop dflags c (finalCompKont tid)

            codeGenCompilerGlobals tid (tickHdl cinfo)
                                       (procHdl cinfo) maybe_tv ta tb
            return cinfo
  where
    checkInOutFiles :: Ty -> Ty -> Cg (BufTy, BufTy)
    checkInOutFiles (TBuff bta) (TBuff btb) = return (bta, btb)
    checkInOutFiles _tin _tout =
        fail $ "Missing read/write? Can't determine input or output type(s)."

    checkCompType :: CTy -> Cg (Maybe Ty, Ty, Ty)
    checkCompType (CTComp tv ta tb) = return (Just tv, ta, tb)
    checkCompType (CTTrans ta tb)   = return (Nothing, ta, tb)
    checkCompType _ = do
        fail $ "CodeGen error, the type of:\n"
                ++ show c ++ "\n" ++
                "is: " ++ show (compInfo c) ++ "\n" ++
                "but should be a fully applied computation type.\n" ++
                "At location: " ++ (displayLoc . locOf . compLoc) c






codeGenThreadAtomix :: DynFlags
                    -> QueueInfo -- Queues
                    -> Int       -- atomix thread id (used only in atomix) -- TODO: change thread id to Int and unify
                    -> AxAuto SymAtom
                    -> Cg CLabel
codeGenThreadAtomix dflags queues atid automaton
  | isDynFlagSet dflags Pipeline
  = fail "AtomixCodeGen cannot be used simultaneously with Pipeline!"
  | otherwise -- Atomix case
  = do cgIO $ putStrLn ""
       cgIO $ putStrLn (" **** Creating thread " ++ show atid)
       cgIO $ putStrLn ""
       cgAutomaton dflags atid queues automaton
  





codeGenProgram :: DynFlags                -- Flags
               -> CompCtxt                -- Context
               -> [(PP.ThreadId, Comp)]   -- Threads
               -> [Ty]                    -- Buftys (Between threads)
               -> (Ty,Ty)                 -- Main input and output type
               -> Cg ()
codeGenProgram dflags shared_ctxt
               tid_cs bufTys (in_ty,yld_ty)
  | not (isDynFlagSet dflags AtomixCodeGen)
  = withModuleName module_name $
    do { codeGenContexts >>= appendTopDecls
       ; (_,moreinitstms) <- codeGenSharedCtxt dflags True shared_ctxt $
           do { forM_ tid_cs $ \(tid,c) -> codeGenThread dflags tid c
              ; if pipeline_flag then
                  do { -- Just to make the SORA code happy we need
                       -- to implement a dummy wpl_go
                       -- In reality the set_up_threads() function uses
                       -- thread0,thread1,...
                     ; let wpl_go_dummy_def
                              = [cedecl| int wpl_go() { exit (-1); } |]
                     ; appendTopDefs [wpl_go_dummy_def]

                       -- Emit the appropriate wpl_set_up_threads()
                       -- definition for SORA code to work
                     ; appendTopDefs $
                       ST.thread_setup affinity_mask module_name bufTys tids
                     }
                else
                   -- In this case we know that wpl_go /is/ going to be
                   -- the main function
                   appendTopDefs $ ST.thread_setup_shim module_name
              }
          -- Emit buf init and fins (once all C types are defined)
       ; cgExtBufInitsAndFins (in_ty,yld_ty) (getName dflags)
          -- Finally emit wpl_global_init()
       ; lut_init_stms <- getLUTHashes >>=
                              (return . map (lgi_lut_gen . snd))

       ; allocation_stmts <- getGlobalWplAllocated
       ; codeGenWPLGlobalInit (allocation_stmts ++ lut_init_stms ++ moreinitstms) module_name
       }

  | [(_, c)] <- tid_cs
  = withModuleName module_name $
    do { codeGenContexts >>= appendTopDecls
       ; (_maybe_tv, ta, tb) <- checkCompType (ctComp c)
       ; (bta, btb) <- checkInOutFiles ta tb
       ; (_,moreinitstms) <- codeGenSharedCtxt dflags True shared_ctxt $
         withThreadId "" $ do
           sym <- getSymEnv
           (ac,rnst) <- cgIO $ zirToAtomZir dflags sym c
           (automaton :: AxAuto SymAtom) 
                 <- cgIO $ automatonPipeline dflags sym (bufty_ty bta) (bufty_ty btb) ac
           let queues = (extractQueues automaton)
           appendTopDecl [cdecl| $ty:(namedCType "long volatile") *barr_hist1 = NULL; |]
           appendTopDecl [cdecl| $ty:(namedCType "long volatile") *barr_hist2 = NULL; |]
           cgAutomatonDeclareAllGlobals dflags rnst queues automaton  $ 
             if no_threads > 1 then 
               -- Multi-threaded case
               do { forM_ (interval (no_threads - 1)) $ \atid -> 
                      mkAtomixRuntime dflags (Just ((getName dflags) ++ "thread" ++ show atid)) $
                      codeGenThreadAtomix dflags queues atid automaton
                  ; do { -- Just to make the SORA code happy we need
                       -- to implement a dummy wpl_go
                       -- In reality the set_up_threads() function uses
                       -- thread0,thread1,...
                     ; let wpl_go_dummy_def
                              = [cedecl| int wpl_go() { exit (-1); } |]
                     ; appendTopDefs [wpl_go_dummy_def]

                       -- Emit the appropriate wpl_set_up_threads()
                       -- definition for SORA code to work
                     ; appendTopDefs $
                       -- ST.thread_setup affinity_mask module_name bufTys tids
                       ST.thread_setup_atomix affinity_mask module_name no_threads
                     }
                  }
             else
               -- Single-threaded case
               do { mkAtomixRuntime dflags (Just ((getName dflags))) $
                    codeGenThreadAtomix dflags queues 0 automaton
                   -- In this case we know that wpl_go /is/ going to be
                   -- the main function
                  ;  appendTopDefs $ ST.thread_setup_shim module_name
                  }
          -- Emit buf init and fins (once all C types are defined)
       ; cgExtBufInitsAndFins (in_ty,yld_ty) (getName dflags)
          -- Finally emit wpl_global_init()
       ; lut_init_stms <- getLUTHashes >>=
                              (return . map (lgi_lut_gen . snd))

       ; allocation_stmts <- getGlobalWplAllocated
       ; codeGenWPLGlobalInit (allocation_stmts ++ lut_init_stms ++ moreinitstms) module_name
       }
  | otherwise
  = fail "Atomix case with multiple pregenerated threads - should not happen"

  where
    no_threads      = getNoAtomThreads dflags
    tids            = map fst tid_cs
    pipeline_flag   = isDynFlagSet dflags Pipeline
    affinity_mask   = getAffinityMask dflags
    module_name     = getName dflags
    interval  0     = [0]
    interval  e     = (interval (e-1)) ++ [e]

    checkInOutFiles :: Ty -> Ty -> Cg (BufTy, BufTy)
    checkInOutFiles (TBuff bta) (TBuff btb) = return (bta, btb)
    checkInOutFiles _tin _tout =
        fail $ "Missing read/write? Can't determine input or output type(s)."

    checkCompType :: CTy -> Cg (Maybe Ty, Ty, Ty)
    checkCompType (CTComp tv ta tb) = return (Just tv, ta, tb)
    checkCompType (CTTrans ta tb)   = return (Nothing, ta, tb)
    checkCompType _ = do
          fail $ "CodeGen error, the type should be TBuff"
-- TBD: Fix the scope of c and reenable the error message
--        fail $ "CodeGen error, the type of:\n"
--                ++ show c ++ "\n" ++
--                "is: " ++ show (compInfo c) ++ "\n" ++
--                "but should be a fully applied computation type.\n" ++
--                "At location: " ++ (displayLoc . locOf . compLoc) c
