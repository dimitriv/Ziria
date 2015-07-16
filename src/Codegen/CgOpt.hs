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
{-# LANGUAGE RecordWildCards #-}
module CgOpt where

import Prelude

import Rebindables
import Opts
import AstExpr
import AstComp
import AstUnlabelled

import CtExpr
import CtComp

import PpComp
import PpExpr
import qualified GenSym as GS
import CgHeader
import CgRuntime
import CgMonad
import CgTypes
import CgExpr
import CgLUT

import Control.Monad.Writer
import qualified Data.DList as DL
import qualified Data.List
import Data.Loc
import Data.Monoid
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Set as S
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ 
import Data.Maybe

import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO(..))

import Data.List ( nub )

import CgFun
import Outputable

import CgValDom
import CgCmdDom
------------------------------------------------------------------------------
-- | Computation Code Generation
------------------------------------------------------------------------------

compGenBind :: DynFlags
            -> GName CTy
            -> [GName (CallArg Ty CTy)]
            -> Comp
            -> CompFunGen
compGenBind dflags f params c1 args k = do

    appName <- nextName $ "__" ++ (name f) ++ "_"
    let appId = appName

    let (eparams,cparams) = partitionParams params
    let (eargs, cargs)    = partitionCallArgs args

    (args_decls, args_stms, code_args) <- inNewBlock $
                                          mapM (codeGenExp dflags) eargs


    let eparamsEnv = zip eparams code_args

    let new_c1 = foldl (\xc (nm,ce) ->
                  cLet (compLoc c1) nm ce xc) c1 (zip cparams cargs)

    pushName appName

    c1info <- extendVarEnv eparamsEnv $ do
              codeGenComp dflags new_c1 k

    appendDecls args_decls

    return $ c1info { compGenInit = -- Wrong! codeDecls args_decls  `mappend`
                                    codeStmts args_stms   `mappend`
                                    compGenInit c1info
                    }

mkRepeat :: DynFlags
         -> Comp
         -> CompInfo
         -> CompKont
         -> Cg CompInfo
mkRepeat dflags c cinfo k =
    codeGenComp dflags c (k { kontDone = cDoneKont})
  where
    cDoneKont = do
        emitCode (compGenInit cinfo)
        if canTick cinfo
          then appendStmt  [cstm|goto $id:(tickNmOf (tickHdl cinfo));|]
          else appendStmts [cstms|$id:globalWhatIs = SKIP;
                                  goto l_IMMEDIATE;|]

mkWhile :: DynFlags
        -> Exp         -- Test
        -> Comp        -- Body
        -> CompInfo    -- Self
        -> CompKont    -- Outer continuation
        -> Cg CompInfo
mkWhile dflags etest cbody cinfo k
  = mkBranch dflags etest cbody k' cretunit k'' csp
  where
    csp = compLoc cbody
    k'  = k { kontDone = doneKont }
    k'' = k { kontDone = doneKont' k }
    cretunit = cReturn csp AutoInline $ eVal csp TUnit VUnit

    doneKont = do emitCode (compGenInit cinfo)
                  if canTick cinfo
                  then appendStmt  [cstm|goto $id:(tickNmOf (tickHdl cinfo));|]
                  else appendStmts [cstms|$id:globalWhatIs = SKIP;
                                          goto l_IMMEDIATE;|]

    doneKont' k = kontDone k


mkBranch :: DynFlags
         -> Exp
         -> Comp  -> CompKont  -- we give each branch his own continuation
         -> Comp  -> CompKont  -- we give each branch his own continuation
         -> SrcLoc
         -> Cg CompInfo
mkBranch dflags e c1 k1 c2 k2 csp
  = do { branchName <- nextName ("__branch_" ++ (getLnNumInStr csp))
       ; let branch_id = branchName

       ; c1info <- codeGenCompTop dflags c1 k1
       ; c2info <- codeGenCompTop dflags c2 k2
       ; (edecls,estmts,ce) <- inNewBlock $ codeGenExp dflags e

       ; branch_var_name <- freshVar ("__branch_var_" ++ (getLnNumInStr csp))
       ; let branch_var = branch_var_name

         -- OK! DV: Checked initialization in compInit
       ; codeGenDeclVolGroup branch_var tint ZeroOut >>= appendDeclPkg
        -- The init function for branch determines the value of
        -- [branch_var], which is used in tick and process to direct control
        -- flow
        --
        -- Tick jumps to c1_tick or c2_tick, depending on the value of
        -- branch_var
        --
        -- Ditto for process
       ; appendLabeledBlock (tickNmOf branch_id) $
            appendStmt [cstm|if ($id:branch_var == 0) {
                               goto $id:(tickNmOf $ tickHdl c1info);
                             } else {
                               goto $id:(tickNmOf $ tickHdl c2info);
                             }
                            |]

       ; appendLabeledBlock (processNmOf branch_id) $
            appendStmt [cstm|if ($id:branch_var == 0) {
                               goto $id:(processNmOf $ procHdl c1info);
                             } else {
                               goto $id:(processNmOf $ procHdl c2info);
                             }
                            |]
         -- [if e then c1 else c2] is tickable if either c1 of c2 is

       ; return (mkCompInfo branch_id (canTick c1info || canTick c2info))
                   { compGenInit = codeDecls edecls `mappend`
                                   codeStmts estmts `mappend`
                                   if ce
                                   then (codeStmt [cstm|$id:branch_var = 0;|] `mappend`
                                         compGenInit c1info
                                        )
                                   else (codeStmt [cstm|$id:branch_var = 1;|] `mappend`
                                         compGenInit c2info
                                        )
                   }
       }

{- Note [Branch Initialization]
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We call the initialization code of /either/ c1 /or/ c2 depending on
the evaluation of the condition e (ce). This means that there may
appear to be situations where some local state of c2 may appear as
being used uninitialized and similarly for c1. Hence all branch and
state combinator local state is declared with codeGenDeclVolGroup_init
with some default (unimportant) value.

Typical example is:

   lbl1:
         if cond {
             some_branch_var = 1;
             branch1_local_state = X;
      } else {
          some_branch_var = 2;
          branch2_local_state = Y;
      }
      goto tick_55;

   tick_55:
       if (some_branch_var == 1)
            goto process34
       else
            use branch2_local_state

   process35:
      use branch1_local_state

If the compiler analysis is not able to detect that the only way to
reach process35 is via paths that set the branch1_local_state, we get
an error for potentially uninitialized state. We know this cannot
possibly happen but we don't want to have to initialize both branches
for ticking/processing only one!

-}





-- We need to calculate a fixpoint when compiling recursive computers like
-- Repeat and Until. What we really need is the tick and process handles for the
-- computation while we are compiling it so we can call them recursively. Since
-- their names are gensym'd, we cheat by running the compilation twice; the
-- first run is used only to calculate the handle names. This requires making
-- sure both runs have the same initial monadic state. In particular, the source
-- of numbers for gensym'ing must be the same for both runs.
codeGenFixpoint :: (CompInfo -> CompKont -> Cg CompInfo)
                -> CompKont
                -> Cg CompInfo
codeGenFixpoint f k = do
    cinfo <- withClonedState $ f (mkCompInfo "dummy" True) k
    f cinfo k

------------------------------------------------------------------------------
-- | Read/Write to thread comm. buffers
------------------------------------------------------------------------------

readCode :: Ty
         -> SrcLoc
         -> String
         -> ReadType
         -> CompKont
         -> Cg CompInfo
readCode ty csp buf_id read_type k = do
    rdSrcName <- nextName ("__readsrc_" ++ getLnNumInStr csp)
    let prefix = rdSrcName

    let (bufget_suffix, getLen) = getTyPutGetInfo ty
    -- Read input using the [ts_get] family of functions instead of [buf_put*]
    -- These functions are defined in [csrc/ThreadSeparator.cpp]
    let bufGetF = "ts_get" -- ++ bufget_suffix

    let yh = yieldHdl k

    let buf_id_as_int :: Integer
        buf_id_as_int = read buf_id

    yldTmp <- nextName ("__yldarr_" ++ getLnNumInStr csp)
    let yldTmpName = yldTmp

    cgIO $ putStrLn $ "readCode! (before): " ++ show ty

    let is_struct_ptr = isStructPtrType ty
    let yldty_is_pointer = isArrayTy ty || is_struct_ptr

    cgIO $ putStrLn "readCode! (after) "

    when yldty_is_pointer $
        -- Allocate a new array buffer
        appendCodeGenDeclGroup yldTmpName ty ZeroOut

    appendLabeledBlock (tickNmOf prefix) $
     do { appendStmt [cstm| while (ts_isEmpty($id:buf_id)) {
                              if (ts_isFinished($id:buf_id)) return 3;
                            }
                         |]
        ; if yldty_is_pointer
                   -- Memcpy from ThreadSeparator buffer into new array buffer and set pointer
                   then appendStmts [cstms|$id:(bufGetF)($id:buf_id, (char *) $id:yldTmpName);
                                           $id:(yldValOf yh) = $id:yldTmpName;
                                          |]
                   else appendStmt [cstm| $id:(bufGetF)($id:(buf_id), (char *) & $id:(yldValOf yh)); |]
        ; kontYield k }

{-
        if [cexp|ts_isEmpty($id:buf_id)|]
        then if [cexp|ts_isFinished($id:buf_id)|]
             then appendStmt [cstm|//CONTINUE is a flag checked by the multithread version
                                   //of the outermost go wrapper.  Here we return
                                   //CONTINUE+buf_id to indicate that a FINISHED
                                   //signal was detected on channel buf_id.
                                   //See [CgSetupThreads.hs] for more information.
                                   return $int:(cONTINUE + buf_id_as_int);
                                  |]

             -- See Note [Standalone Reads], Astcomp
             else if readJumpToConsumeOnEmpty read_type
                  then kontConsume k
                  else appendStmts [cstms|$id:globalWhatIs = SKIP;
                                          goto l_IMMEDIATE;
                                         |]

        else do if yldty_is_pointer
                   -- Memcpy from ThreadSeparator buffer into new array buffer and set pointer
                   then appendStmts [cstms|$id:(bufGetF)($id:buf_id, (char *) $id:yldTmpName);
                                           $id:(yldValOf yh) = $id:yldTmpName;
                                          |]
                   else appendStmt [cstm| $id:(bufGetF)($id:(buf_id), (char *) & $id:(yldValOf yh)); |]
                kontYield k
-}

    appendLabeledBlock (processNmOf prefix) $
         appendStmt [cstm|/* Process immediately jumps to read_internal tick */
                          goto $id:(tickNmOf prefix);|]

    return (mkCompInfo prefix True)

writeCode :: Ty
          -> SrcLoc
          -> String
          -> CompKont
          -> Cg CompInfo
writeCode ty csp buf_id k = do
    wrSnkName <- nextName ("__writesnk_" ++ (getLnNumInStr csp))
    let prefix = wrSnkName
        (bufput_suffix, putLen) = getTyPutGetInfo $ ty

    -- Write using the [ts_put] family of functions instead of [buf_put*]
    -- These functions are defined in [csrc/buf_*.h]
    let bufPutF = "ts_put" -- ++ bufput_suffix

    let ih = inHdl k

    -- cgIO $ putStrLn $ "writeCode (before): " ++ show cty0

    let is_struct_ptr = isStructPtrType ty
    let inty_is_pointer = isArrayTy ty || is_struct_ptr

    -- cgIO $ putStrLn $ "writeCode (after)"

    -- First, check if the write buffer is full.
    -- If it is, keep processing lower part of pipeline
    -- by jumping to Immediate w/ whatIs=SKIP.
    -- Otherwise, execute consume continuation.
    --
    -- [ts_put] to buffer [s] and loop!
    appendLabeledBlock (tickNmOf prefix) $
       do { appendStmt [cstm| while(ts_isFull($id:buf_id)) ; |]
          ; kontConsume k }
{-
        if [cexp|ts_isFull($id:buf_id)|]
        then appendStmts [cstms|$id:globalWhatIs = SKIP;
                                // printf("full\n");
                                goto l_IMMEDIATE;
                               |]
        else kontConsume k
-}

    let cinexp = if inty_is_pointer
                 then [cexp|(char *) ($id:(inValOf ih))|]
                 else [cexp|(char *) & ($id:(inValOf ih))|]

    appendLabeledBlock (processNmOf prefix) $
        appendStmts [cstms|($id:bufPutF)($id:(buf_id), $cinexp);
                           $id:globalWhatIs = SKIP;
                           goto l_IMMEDIATE;|]

    -- NB: make sure we tick this component!
    return (mkCompInfo prefix True)

codeGenCompTop :: DynFlags
               -> Comp
               -> CompKont
               -> Cg CompInfo
codeGenCompTop dflags comp k = do
    when (isDynFlagSet dflags Debug) $ 
      verbose dflags $ 
        vcat [ text $ "Generating code for component: " ++ compShortName comp
             , text "Location: " <+> ppr (compLoc comp)

             , text "Component: " <+> ppr comp ]
    codeGenComp dflags comp k

------------------------------------------------------------------------------
-- | Main code generation function
------------------------------------------------------------------------------

codeGenComp :: DynFlags
            -> Comp
            -> CompKont
            -> Cg CompInfo
codeGenComp dflags comp k =
    go comp
  where
    go :: Comp -> Cg CompInfo


    go (MkComp (Mitigate _ bty i1 i2) csp ()) = do
        -- Assume i1 `mod` i2 = 0
        mitName <- nextName ("__mit_" ++ (getLnNumInStr csp))
        let prefix = mitName
        let ih = inHdl k
        let yh = yieldHdl k
        let mit_st = prefix ++ "_mit_state"
        let buf = prefix ++ "_mit_buff"

        genOrigin csp

        if (i1 >= i2)
          then do { let d = i1 `div` i2

                    -- declare the state of the mitigator
                    -- OK! DV: checked initialization in compInit
                  ; appendDeclPkg =<<
                      codeGenDeclVolGroup mit_st tint (InitWith [cinit|$int:d|])

                  ; let arrty = TArray (Literal i1) bty
                        leninfo = if i2 == 1 then LISingleton else LILength i2
                        rety = if i2 == 1 then bty else TArray (Literal  i2) bty

                  ; appendLabeledBlock (tickNmOf prefix) $ do
                        if [cexp| $id:mit_st >= $int:d |] then kontConsume k
                        else do
                           cres <- cgArrRead_chk rety
                                         [cexp|$id:(inValOf ih)|]
                                         (AIdxMult i2 [cexp|$id:mit_st|])
                                         leninfo
                           -- NB: cres may contain mit_st variable, which will be mutated
                           -- hence it is really important that we set yldVal and /then/
                           -- mutate mit_st, not the other way around.
                           appendStmt [cstm|$id:(yldValOf yh) = $cres; |]
                           appendStmt [cstm|$id:mit_st++; |]

                           kontYield k

                  ; appendLabeledBlock (processNmOf prefix) $ do

                        cres <- cgArrRead_chk rety
                                              [cexp|$id:(inValOf ih)|]
                                              (AIdxStatic 0)
                                              leninfo

                        appendStmt [cstm|$id:(yldValOf yh) = $cres;|]
                        appendStmt [cstm|$id:mit_st = 1;|]
                        kontYield k

                  ; return (mkCompInfo prefix True) -- can tick

                  }

          else do { let d = i2 `div` i1

                    -- declare the state of the mitigator
                    -- OK DV: checked initialization 
                  ; appendDeclPkg =<<
                       codeGenDeclVolGroup mit_st tint ZeroOut

                  ; DeclPkg buf_decl buf_inits <- codeGenDeclGroup buf (TArray (Literal i2) bty) ZeroOut
                  ; appendDecl buf_decl



                    -- trivial tick()
                  ; appendLabeledBlock (tickNmOf prefix) $ do
                           kontConsume k

                  ; let arrty   = TArray (Literal i2) bty
                        leninfo = if i1 == 1 then LISingleton else LILength i1
                        rety = if i1 == 1 then bty else TArray (Literal i1) bty

                  ; appendLabeledBlock (processNmOf prefix) $ do
                           -- buff[mit_st,leninfo] := in_val
                           cgArrWrite_chk rety [cexp|$id:buf|]
                                               (AIdxMult i1 [cexp|$id:mit_st|])
                                               leninfo
                                               [cexp|$id:(inValOf ih)|]
                           appendStmt [cstm| $id:mit_st++;|]
                           if [cexp| $id:mit_st >= $int:d|] then do
                              appendStmt [cstm|$id:(yldValOf yh) = $id:buf;|]
                              appendStmt [cstm|$id:mit_st = 0;|]
                              kontYield k
                           else
                               kontConsume k

                  ; let cnfo = mkCompInfo prefix False
                  ; return cnfo { compGenInit = codeStmts buf_inits `mappend` compGenInit cnfo }
                  }

    go (MkComp (Map _p nm) csp ()) = do

        mapName <- nextName ("__map_" ++ (getLnNumInStr csp))
        let prefix = mapName

        let ih = inHdl k
        let yh = yieldHdl k

        -- NB: we need to reuse the codegen for "call" except that we
        -- don't really have a variable name. So we make up a new
        -- variable name called the same way as the in-value. This is
        -- not entirely satisfactory, admittedly.

        let invalloc = csp
            TArrow [(GArgTy invalty _invalmut)] _ = nameTyp nm
        let invalname = MkName { name    = inValOf ih
                               , uniqId  = MkUniq $ inValOf ih -- yikes
                               , nameTyp = invalty
                               , nameMut = Imm
                               , nameLoc = invalloc }
            invalarg  = eVar invalloc invalname

        let ecall     = eCall invalloc nm [invalarg]

        genOrigin csp

        appendLabeledBlock (tickNmOf prefix) $
            kontConsume k

        appendLabeledBlock (processNmOf prefix) $ do
          cresult <- extendVarEnv [(invalname, [cexp|$id:(name invalname)|])] $
                     codeGenExp dflags ecall
          appendStmt [cstm|$id:(yldValOf yh) = $cresult;|]
          kontYield k

        return (mkCompInfo prefix False)

    go c@(MkComp (Emits e) csp ()) = do
        emitName <- nextName ("__emits_"  ++ (getLnNumInStr csp))

        let prefix   = emitName
        let stateVar = prefix ++ "_state"
        let expVar   = prefix ++ "_exp"

        let yh = yieldHdl k
        let dh = doneHdl k

        -- liftIO $ putStrLn $ "info e   = " ++ show (info e)

        -- liftIO $ putStrLn $ "actual e = " ++ show e

        (yldTy, n) <- checkArrayIsLiteral (ctExp e)

        -- NB: We need to copy the emitted array to local storage here since the
        -- array data is created in a local variable belonging to a function and
        -- we cannot guarantee it will stay there by the time emits is done.

        -- OK! 
        appendDeclPkg =<< codeGenDeclVolGroup stateVar tint ZeroOut
        DeclPkg evar_decl evar_stms <- codeGenDeclGroup expVar (ctExp e) ZeroOut
        appendDecl evar_decl


        genOrigin csp
        -- Must generate emit_process since runtime code will use name (even
        -- if it never calls the function) when "emit" is the toplevel
        -- program
        appendLabeledBlock (tickNmOf prefix) $ do
            if [cexp|$id:stateVar == 0|]
              then do ce <- codeGenExp dflags e
                      assignByVal (ctExp e) [cexp|$id:expVar|] ce
              else return ()

            if [cexp|$id:stateVar < $int:n|]
              then do ce <- cgArrRead_chk yldTy
                               [cexp|$id:expVar|]
                               (AIdxCExp [cexp|$id:stateVar|]) LISingleton

                      assignByVal yldTy [cexp|$id:(yldValOf yh)|] ce
                      appendStmts [cstms|$id:globalWhatIs = YIELD;
                                         ++$id:stateVar;
                                        |]
                      kontYield k

              else do appendStmts [cstms|$id:(doneValOf dh) = UNIT;
                                         $id:globalWhatIs   = DONE;
                                        |]
                      kontDone k

        appendLabeledBlock (processNmOf prefix) $
            appendStmts [cstms|/* Emit process will never be called/doesn't matter what we return here */
                               return IMMEDIATE;
                              |]

        return (mkCompInfo prefix True) {compGenInit = codeStmt [cstm| $id:stateVar = 0;|] `mappend` 
                                                       codeStmts evar_stms
                                        }
      where
        checkArrayIsLiteral :: Ty -> Cg (Ty,Int)
        checkArrayIsLiteral (TArray (Literal n) bt) =
            return (bt,n)
        checkArrayIsLiteral _ =
            fail $ "CodeGen error with emits, this should have been picked by the type checker"

    go (MkComp (WriteSnk ty) csp _) = do
        -- NB: ignore the optional type annotation of WriteSnk but use the type of the computation instead.
        wrSnkName <- nextName ("__writesnk_" ++ (getLnNumInStr csp))
        let prefix = wrSnkName

        let (bufput_suffix, putLen) = getTyPutGetInfo ty
        let bufPutF = "buf_put" ++ bufput_suffix

        let ih = inHdl k

        genOrigin csp

        appendLabeledBlock (tickNmOf prefix) $
            kontConsume k

        buf_context  <- getBufContext
        global_params <- getGlobalParams

        appendLabeledBlock (processNmOf prefix) $
            if isArrayTy ty then
                appendStmts [cstms|$id:bufPutF($id:global_params, $id:buf_context, $id:(inValOf ih), $putLen);
                                   $id:globalWhatIs = SKIP;
                                   goto l_IMMEDIATE;
                                  |]
            else
              if isUserStructTy ty then
                  appendStmts [cstms|$id:bufPutF($id:global_params, $id:buf_context, (void *) (&$id:(inValOf ih)));
                                     $id:globalWhatIs = SKIP;
                                     goto l_IMMEDIATE;
                                    |]
              else
                  appendStmts [cstms|$id:bufPutF($id:global_params, $id:buf_context, $id:(inValOf ih));
                                     $id:globalWhatIs = SKIP;
                                     goto l_IMMEDIATE;
                                    |]

        return (mkCompInfo prefix False)

    go (MkComp (ReadSrc ty) csp _) = do
        -- NB: ignore the optional type annotation of WriteSnk but use the type of the computation instead.
        rdSrcName <- nextName ("__readsrc_" ++ (getLnNumInStr csp))
        let prefix = rdSrcName
        let (bufget_suffix, getLen) = getTyPutGetInfo ty
        let bufGetF   = "buf_get" ++ bufget_suffix
        let bufGetEOF = "isEOF"   ++ bufget_suffix

        let yh = yieldHdl k

        -- Allocate our own storage
        -- DV: nextName!?
        yldTmp <- nextName ("__yldarr_" ++ getLnNumInStr csp)
        let yldTmpName = yldTmp

        genOrigin csp

        if isArrayTy ty then
          -- Allocate a new array buffer
          do appendCodeGenDeclGroup yldTmpName ty ZeroOut
             appendLabeledBlock (tickNmOf prefix) $ do
                   buf_context <- getBufContext
                   global_params <- getGlobalParams
                   appendStmts [cstms| if ($id:(bufGetF)($id:global_params, $id:buf_context,
                                                         $id:yldTmpName,$(getLen)) == GS_EOF)
                                         return $int:(cONTINUE - 1);
                                       $id:(yldValOf yh) = $id:yldTmpName;
                                           //CONTINUE is a flag checked by the multithread version
                                           //of the outermost go wrapper.  In the single-threaded
                                           //case, it is just the return value 1.  Here we return
                                           //CONTINUE - 1 to indicate EOF on channel -1 (the input
                                           //channel).  See [CgSetupThreads.hs] for more information.
                              |]
                   kontYield k
        else
             appendLabeledBlock (tickNmOf prefix) $ do
                   buf_context <- getBufContext
                   global_params <- getGlobalParams
                   appendStmt [cstm| if ($id:(bufGetF)($id:global_params, $id:buf_context, & $id:(yldValOf yh)) == GS_EOF)
                                           return $int:(cONTINUE - 1);
                              |]
                   kontYield k

        appendLabeledBlock (processNmOf prefix) $
            appendStmt [cstm|/* Process will never be called like emit,
                              * it doesn't matter what we return here
                              */
                             return IMMEDIATE;
                            |]

        return (mkCompInfo prefix True)


    -- Write to an internal ThreadSeparator buffer [buf_id].
    go (MkComp (WriteInternal ty buf_id) csp ())
      = writeCode ty csp buf_id k


    go (MkComp (ReadInternal ty buf_id tp) csp ()) =
        readCode ty csp buf_id tp k

    go (MkComp (Emit e) csp ()) = do
        emitName <- nextName ("__emit_" ++ (getLnNumInStr csp))
        let prefix   = emitName
        let stateVar = prefix ++ "_state"

        let yh = yieldHdl k
        let dh = doneHdl k

        -- BOZIDAR: The code below doesn't work, e.g. in test
        -- "times-opt-foo.wpl.c" where we do vector take1: (vect_xa_14 : arr[2]
        -- int) <- take1; because (yldValOf yh) is not malloced, and EAssign
        -- does memcpy which causes segmentation fault The current version
        -- effectively assigns by reference, which might be a feature But this
        -- won't work for special types in future for which assignments takes
        -- more than just: $id:(yldValOf yh) = $eCode; FIX THIS:

        -- Must generate emit_process since runtime code will use name (even if
        -- it never calls the function) when "emit" is the toplevel program

        -- OK! 
        appendDeclPkg =<< codeGenDeclVolGroup stateVar tint ZeroOut

        mapM_ appendStmt $
          [cstms| $comment:("/* " ++ displayLoc (locOf csp) ++ " */") |]

        appendLabeledBlock (tickNmOf prefix) $
            if [cexp|!$id:stateVar|]
            then do ce <- codeGenExp dflags e
                    appendStmt [cstm|$id:(yldValOf yh) = $ce;|]
                    appendStmt [cstm|$id:stateVar = TRUE;|]
                    kontYield k
            else do appendStmt [cstm|$id:(doneValOf dh) = UNIT;|]
                    kontDone k

        appendLabeledBlock (processNmOf prefix) $
            appendStmt [cstm|// Emit process will never be called/doesn't matter what we return here
                             return IMMEDIATE;
                            |]

        return (mkCompInfo prefix True) { compGenInit = codeStmt [cstm|$id:stateVar = FALSE;|] }

    go (MkComp (Let x c1 c2) csp _) =
        codeGenSharedCtxt_ dflags False (CLet csp x c1 Hole) $
        codeGenCompTop dflags c2 k

    go (MkComp (LetStruct sdef c2) csp _) =
        codeGenSharedCtxt_ dflags False (CLetStruct csp sdef Hole) $
        codeGenCompTop dflags c2 k

    go (MkComp (LetE x fi e c2) csp _) = do
        (cinfo,stms) <-
            codeGenSharedCtxt dflags False (CLetE csp x fi e Hole) $
            codeGenCompTop dflags c2 k
        -- Make sure we init this guy, regardless of whether c1 is init'able
        return $ cinfo { compGenInit = codeStmts stms `mappend`
                                       compGenInit cinfo }

    -- CL
    go (MkComp (LetERef x y c2) csp _) = do
        (cinfo,stms) <- codeGenSharedCtxt dflags False (CLetERef csp x y Hole) $
                        codeGenCompTop dflags c2 k
        return $ cinfo { compGenInit = codeStmts stms `mappend`
                                       compGenInit cinfo }

    go (MkComp (LetHeader fdef c1) csp _)
      = codeGenSharedCtxt_ dflags False (CLetHeader csp fdef Hole) $
        codeGenCompTop dflags c1 k
    --
    go (MkComp (LetFunC f params c1 c2) csp ()) =
        codeGenSharedCtxt_ dflags False (CLetFunC csp f params c1 Hole) $
        codeGenCompTop dflags c2 k

    go (MkComp (BindMany c1 []) csp ()) =
        codeGenCompTop dflags c1 k

    go cb@(MkComp (BindMany c1 nms_cs) csp ()) = do
        c1Name <- freshLabel ("__bnd_fst_" ++ (getLnNumInStr csp))
        let c1id = c1Name
        bmName <- nextName ("__bind_" ++ (getLnNumInStr csp))
        let bmNamePref = bmName

        let yh = yieldHdl k
        let dh = doneHdl k

        branch_var_name <- freshVar ("__branch_var_" ++ (getLnNumInStr csp))
        let branch_var = branch_var_name

        -- OK 
        appendDeclPkg =<< codeGenDeclVolGroup branch_var tint ZeroOut

        -- INVARIANT: l is nonempty
        let genGotoSwitch :: (Int, CompInfo) -> C.Stm
            genGotoSwitch (n, cinfo) = [cstm|case $int:n: goto $id:((processNmOf . procHdl) cinfo); |]

        consumeKontCode <- collectStmts_ $ kontConsume k

        let genGotoSwitchTick :: (Int, CompInfo) -> C.Stm
            genGotoSwitchTick (n, cinfo)
                | canTick cinfo = [cstm|case $int:n: goto $id:((tickNmOf . tickHdl) cinfo); |]
                | otherwise     = [cstm|case $int:n: { $stms:consumeKontCode } |]

        -- Compilation plan: generate a tick and process function for the entire
        -- block.  Inside tick and process, case switch on the integer variable
        -- [branch_var] to determine which subcomponent to jump to next.

        -- When a subcomponent is DONE, it must increment [branch_var] modulo
        -- [num_branches], in order to divert control to the next block in the
        -- Bind chain.

        let go :: String -> Int -> (Comp, CLabel)
               -> [(EId, Comp)] -> Cg [CompInfo]
            go branch_var num_branches (c1,c1Name) ((nm,c2):rest) = do
                let vTy = ctDoneTyOfComp c1
                let is_take (Take1 {}) = True
                    is_take _          = False

                new_dh <- freshName ("__dv_tmp_"  ++ (name nm) ++ "_" ++ (getLnNumInStr csp)) vTy Mut
                let new_dhval = doneValOf $ name new_dh
                DeclPkg dv_decl dv_inits <- codeGenDeclGroup new_dhval vTy ZeroOut
                appendDecl dv_decl 
{- 
                appendDeclPkg =<<
                    -- Note [Take Optimization]
                    -- If the component is a Take1 then we can simply make
                    -- sure that we return the address of the in value, and
                    -- hence we don't have to declare storage for doneVal.

                    -- Alas this optimization IS WRONG! 
                    -- read >>> seq { x <- take[array]; y <- take[array]; ... }
                    -- If we do the optimization, the moment we read 'y' we will
                    -- invalidate the memory that 'x' is pointing to! BUG! 
                     {-
                      if is_take (unComp c1) && isArrayTy vTy then
                         return [cdecl| $ty:(codeGenTyAlg vTy) $id:new_dhval;|]
                      else
                     -}
                        codeGenDeclGroup new_dhval vTy ZeroOut
-}

                let ce = [cexp|$id:new_dhval |]
                c2Name <- freshLabel ("__bnd_rest_" ++ (getLnNumInStr csp))
                rest_info <- extendVarEnv [(nm,ce)] $
                             go branch_var num_branches (c2,c2Name) rest
                let c2info = head rest_info -- rest_info is always nonempty

                let c2TickId = tickNmOf $ tickHdl c2info
                let c2ProcId = processNmOf $ procHdl c2info

                -- Optimized: when c1 is DONE, c1 calls c2 tick directly - after
                -- setting [branch_var] to [branch_var+1]
                let c1DoneKont = do
                    appendStmt [cstm|$id:branch_var = $id:branch_var + 1;|]
                    emitCode (compGenInit c2info)
                    -- Instead of returning to the outer loop, we tick c2
                    -- directly.
                    appendStmt [cstm|goto $id:(tickNmOf (tickHdl c2info));|]

                pushName c1Name
                c1info <- codeGenCompTop dflags c1 (k { kontDone = c1DoneKont, doneHdl = name new_dh})
                let c1info' = c1info { compGenInit = codeStmts dv_inits `mappend` compGenInit c1info }
                return $ c1info' : rest_info

            go branch_var num_branches (c1,c1Name) [] = do
                pushName c1Name
                c1info <- codeGenCompTop dflags c1 k
                let c1info' = c1info
                return [c1info']

        -- INVARIANT: switchTbl is always nonempty (look at go below)
        switchTbl <- go branch_var (length nms_cs + 1) (c1,c1Name) nms_cs
        let c1info = head switchTbl

        let switchTblIdx = zip [0..length switchTbl - 1] switchTbl
        -- Generate an init that initializes the leftmost component
        --
        -- And a tick that jumps to the tick label of the subcomputation
        -- labeled [branch_var]
        --
        -- And a process that ... ditto
        genOrigin csp

        appendLabeledBlock (tickNmOf bmNamePref) $
            appendStmt [cstm|switch ($id:branch_var) {
                               $stms:(map genGotoSwitchTick switchTblIdx)
                             }
                            |]

        appendLabeledBlock (processNmOf bmNamePref) $
            appendStmt [cstm|switch ($id:branch_var) {
                               $stms:(map genGotoSwitch switchTblIdx)
                             }
                            |]

        -- BindMany is tickable if any one of its subcomputations is tickable
        return (mkCompInfo bmNamePref (any canTick switchTbl))
                   { compGenInit = codeStmt [cstm|$id:branch_var = 0;|] `mappend`
                                   compGenInit c1info
                   }

    go c@(MkComp (Par parInfo c1 c2) csp ()) = do
        c1Name <- nextName ("__par_c1_" ++ (getLnNumInStr csp))
        c2Name <- nextName ("__par_c2_" ++ (getLnNumInStr csp))
        let c2id = c2Name

--        liftIO $ putStrLn "(Par) Before"

        let yTy  = ctParMid c1 c2

--        liftIO $ putStrLn ("(Par) After: " ++ show yTy)


        -- Allocate intermediate yield buffer
        new_yh <- freshVar ("__yv_tmp_"  ++ (getLnNumInStr csp))
        let new_yhval = yldValOf $ new_yh
            b = isStructPtrType yTy
            yTy_c = codeGenTyOcc_ (Just "calign") yTy
            yv_init = cgInitVal yTy
            ydecl | (b && not (isArrayTy yTy)) -- Not already a pointer
                  = [cdecl| $ty:yTy_c * $id:new_yhval = NULL; |]
                  | isArrayTy yTy
                  = [cdecl| $ty:yTy_c $id:new_yhval = NULL; |]
                  | otherwise
                  = [cdecl| $ty:yTy_c $id:new_yhval = $init:yv_init;|]

        appendDecl ydecl

        -- Translate c1
        let c1YieldKont = appendStmt [cstm|goto $id:(processNmOf c2id);|]
        pushName c1Name
        c1info <- codeGenCompTop dflags c1 $
                  k { kontYield = c1YieldKont, yieldHdl = new_yh }

        -- Translate c2
        -- c2ConsumeKont is (return c1_tick()) if c1 can tick, otherwise the current
        -- consume continuation
        let c2ConsumeKont = if canTick c1info
                            then appendStmt [cstm|goto $id:(tickNmOf (tickHdl c1info));|]
                            else kontConsume k
        -- Yield continuation should remain the same
        -- Done continuation should remain the same
        -- In handle is yield handle of c1

        pushName c2Name
        c2info <- codeGenCompTop dflags c2 (k { kontConsume = c2ConsumeKont, inHdl = new_yh})

        return $ CompInfo { tickHdl    = if canTick c2info then tickHdl c2info else tickHdl c1info
                          , procHdl    = procHdl c1info
                            -- NB: c1 >>> c2 is tickable if either c1 or c2 is
                          , canTick    = canTick c1info || canTick c2info

                          , compGenInit = compGenInit c1info `mappend`
                                          compGenInit c2info
                          }

    go (MkComp (Seq c1 c2) csp csinfo) = do
        let dty = ctDoneTyOfComp c1
        unusedName <- freshName ("__seq_unused_" ++ (getLnNumInStr csp)) dty Imm
        codeGenCompTop dflags (MkComp (mkBind c1 (unusedName, c2)) csp csinfo) k

    go (MkComp (Var nm) csp _) = do
        f <- lookupCompCode nm
        f k

    -- NB: Call is non polymorphic at computation level
    go (MkComp (Call nm args) csp _) = do
        f <- lookupCompFunCode nm
        f args k

    go c@(MkComp (Take1 inty) csp ()) = do
        takeName <- nextName ("__take_" ++ (getLnNumInStr csp))
        let prefix = takeName

        let ih = inHdl k
        let dh = doneHdl k


        let donevalexp = [cexp|$id:(doneValOf dh)|]
        let invalofexp = [cexp|$id:(inValOf ih)|]

        appendLabeledBlock (tickNmOf prefix) $
            kontConsume k

        appendLabeledBlock (processNmOf prefix) $ do
            -- See Note [Take Optimization]
            {- 
            if isArrayTy inty then
                appendStmt [cstm|$id:(doneValOf dh) = $id:(inValOf ih);|]
            else         
            -}
            assignByVal inty donevalexp invalofexp

            -- WHAT!? 
            -- appendStmt [cstm|$id:(doneValOf dh) = $id:(inValOf ih);|]

            appendStmt [cstm|$id:globalWhatIs = DONE;|]
            kontDone k

        return (mkCompInfo prefix True)

    go c@(MkComp (Take elemTy n) csp ()) = do
        takeName <- nextName ("__take_" ++ (getLnNumInStr csp))
        let prefix   = takeName
        let stateVar = prefix ++ "_state"

        let ih = inHdl k
        let dh = doneHdl k

        let aTy = TArray (Literal n) elemTy

        -- OK! 
        appendDeclPkg =<< codeGenDeclVolGroup stateVar tint ZeroOut

        appendLabeledBlock (tickNmOf prefix) $
            kontConsume k

        appendLabeledBlock (processNmOf prefix) $ do
            cgArrWrite_chk elemTy [cexp|$id:(doneValOf dh)|]
                                  (AIdxCExp [cexp|$id:stateVar|]) 
                                  LISingleton 
                                  [cexp|$id:(inValOf ih)|]

            appendStmt [cstm|++$id:stateVar;|]
            if [cexp|$id:stateVar < $int:n|]
              then appendStmts [cstms|$id:globalWhatIs = SKIP;
                                      goto l_IMMEDIATE;
                                     |]
              else do appendStmts [cstms|$id:stateVar = 0;
                                         $id:globalWhatIs = DONE;
                                        |]
                      kontDone k

        return (mkCompInfo prefix True)
                   { compGenInit = codeStmt [cstm|$id:stateVar = 0;|] }

    go (MkComp (Return _ e) csp ()) = do
        retName <-  nextName ("__ret_" ++ (getLnNumInStr csp))
        let prefix = retName

        let dh = doneHdl k

        appendLabeledBlock (tickNmOf prefix) $ do
            ce <- codeGenExp dflags e
            assignByVal (ctExp e) [cexp|$id:(doneValOf dh)|] ce
            appendStmt [cstm|$id:globalWhatIs = DONE;|]
            kontDone k

        appendLabeledBlock (processNmOf prefix) $
            -- NB: return process will never be called/doesn't what we do here
            appendStmt [cstm|goto l_IMMEDIATE;|]

        return (mkCompInfo prefix True)

    -- Compilation plan for branch:
    -- o Allocate an integer variable [branch_var]
    -- o Wrap c1/c2 in tick/process/init functions that jump to c1_tick/c1_process if
    --   branch_var == 0, and to c2_tick/c2_process if branch_var == 0.
    -- o The init function for branch is the only code that modifies branch_var
    --   (to 0 or 1, depending on the value of [e])
    go (MkComp (Branch e c1 c2) csp ()) =
        mkBranch dflags e c1 k c2 k csp -- Pass them the same continuation!

    go (MkComp (Repeat wdth c1) csp ()) = do
        let vTy = ctDoneTyOfComp c1
        new_dh <- freshName ("__dv_tmp_"  ++ (getLnNumInStr csp)) vTy Mut
        let new_dhval = doneValOf $ name new_dh
        appendCodeGenDeclGroup new_dhval vTy ZeroOut
        codeGenFixpoint (mkRepeat dflags c1) (k { doneHdl = name new_dh})

    go (MkComp (Until e c1) csp ()) = do 
        ffst <- freshName "__fst" TBool Mut
        let eflip  = eAssign csp (eVar csp ffst) (eVal csp TBool (VBool False))
            econd1 = eVar csp ffst
            econd2 = eUnOp csp Not e
            econd  = eBinOp csp Or econd1 econd2 
            -- letref ffst := True 
            -- in while ( fst || not e) seq { c1 ; ffst := False } 
            while_comp = cLetERef csp ffst (Just (eVal csp TBool (VBool True))) $
                         cWhile csp econd $ cSeq csp c1 (cReturn csp AutoInline eflip)

        codeGenCompTop dflags while_comp k

        -- let comp = cLetERef csp (
        -- codeGenFixpoint (mkUntil dflags (codeGenExp dflags e) (return ()) c1) k


    go (MkComp (VectComp _ c) _ _) = go c  -- Just ignore vectorization annotation if it has survived so far.

    go (MkComp (While e c1) csp ()) =
        codeGenFixpoint (mkWhile dflags e c1) k


    go cb@(MkComp (Times _ui estart elen nm c1) csp ()) = do
        -- For loops generate code via the following desugaring:
        -- for nm in [estart,elen] c1 
        --    ~~> 
        -- letref nm := estart in while (nm < elen) { c1 ; do {nm := nm+1;} }
        -- 
        -- NB: nm is declared to be immutable, to prevent programmers
        -- from assigning it by mistake, but here we mutate it ourselves. This 
        -- is arguably a bit naughty, and to be completely cosher we'd have to 
        -- change the mutability flag, but the code generator should not really 
        -- care (or check).
        let nmexpr      = eVar csp nm 
            nmexprplus  = eBinOp csp Add nmexpr (eVal csp (nameTyp nm) (VInt 1 Signed))
            eincr  = eAssign csp nmexpr nmexprplus
            efinal = eBinOp csp Add elen estart
            mtest  = eBinOp csp Lt nmexpr efinal
            while_comp = cLetERef csp nm (Just estart) $ 
                         cWhile csp mtest $ 
                         cSeq csp c1 $ cReturn csp NoInline eincr

        codeGenCompTop dflags while_comp k

    go c@(MkComp (Standalone c1) csp ()) = do
       codeGenCompTop dflags c1 k

{-
    go (MkComp c _ (CTArrow {})) =
        fail $ "CodeGen error: BUG!!! " ++ show c

    {-go (MkComp c@(LetHeader {}) _ _) =
        fail $ "CodeGen error, BUG!! " ++ show c-}
    go (MkComp c@(Take {}) _ _) =
        fail $ "CodeGen error, BUG!! " ++ show c
    go (MkComp c@(Times {}) _ _) =
        fail $ "CodeGen error, BUG!! " ++ show c
-}

    go (MkComp c@(Filter {}) _ _) =
        fail $ "CodeGen error, unimplemented: " ++ show c
    go (MkComp c@(Interleave {}) _ _) =
        fail $ "CodeGen error, unimplemented: " ++ show c
{-    go (MkComp c@(Map {}) _ _) =
        fail $ "CodeGen error, unimplemented: " ++ show c
-}
{-
    go (MkComp c _ _) =
        fail $ "CodeGen error, unimplemented: " ++ show c
-}


codeGenSharedCtxt :: DynFlags -> Bool -> CompCtxt -> Cg a -> Cg (a, [C.Stm])
codeGenSharedCtxt dflags emit_global ctxt action = go ctxt action
  where
    go Hole action = action >>= \a -> return (a,[])
    go (CLet csp nm comp ctxt) action
      = extendCompEnv nm (codeGenCompTop dflags comp) $ go ctxt action
    go (CLetStruct csp sdef ctxt) action
      = do { let struct_defn = [cdecl| typedef struct { $sdecls:cfields }
                                                 $id:(struct_name sdef); |]
                 cfields = map decl_field (struct_flds sdef)
                 decl_field (fnm,fty) = codeGenFieldDeclGroup fnm fty

            ; appendStructDef (struct_name sdef) struct_defn
            ; extendTyDefEnv (struct_name sdef) sdef $ go ctxt action
            }
    go (CLetE csp x _ e ctxt) action

      -- TODO TODO TODO! IMPORTANT!
      -- Check what happens with CLetE and threads, i.e when
      -- CLetE is the top-level enclosing context. We should be
      -- generating global declarations.
      --
      = do { (e_decls,e_stmts,ce) <- inNewBlock $ codeGenExp dflags e
           ; let ty = ctExp e
           ; x_name <- freshVar ((name x) ++ "_" ++ (getLnNumInStr csp))
           ; let x_cname = x_name

           ; (DeclPkg x_decl x_stmts) <- codeGenDeclGroup x_cname ty ZeroOut

             -- Declare any declarations from e 
           ; if emit_global then appendTopDecls e_decls else appendDecls e_decls

           ; (_,asgn_stmts) <- inNewBlock_ $
                               assignByVal ty [cexp|$id:x_cname|] ce

           ; if emit_global then appendTopDecl x_decl 
                            else appendDecl x_decl

           ; (a,stms) <- extendVarEnv [(x, [cexp|$id:x_cname|])] $ 
                         go ctxt action

           ; return (a,e_stmts ++ x_stmts ++ asgn_stmts ++ stms)
           }

    -- CL
    -- TODO: Should we do something with _ty here?
    go (CLetERef csp x (Just e) ctxt) action 
      = go (CLetE csp x AutoInline e ctxt) action 
        -- Exactly the same code generation

    go (CLetERef csp x Nothing ctxt) action
      = do { x_name <- freshVar ((name x) ++ "_" ++ (getLnNumInStr csp))
           ; let x_cname = x_name

           ; (DeclPkg d asgn_stmts) <- codeGenDeclGroup x_cname (nameTyp x) ZeroOut

           ; if emit_global then appendTopDecl d 
                            else appendDecl d
           ; (a,stmts) <- extendVarEnv [(x, [cexp|$id:x_cname|])] $ go ctxt action
           ; return (a,asgn_stmts ++ stmts);

           }

    --
    go (CLetFunC csp f params c1 ctxt) action
      = extendFunEnv f (compGenBind dflags f params c1) $
        go ctxt action

    -- Next two defined in CgFun
    go (CLetHeader csp fdef@(MkFun (MkFunExternal {}) _ _) ctxt) action =
        cgFunExternal dflags csp fdef $ go ctxt action
    go (CLetHeader csp fdef@(MkFun (MkFunDefined {}) _ _) ctxt) action =
        cgFunDefined dflags csp fdef $ go ctxt action

    --go (CLetHeader {}) _      = error "BUG: All function kinds covered!"

codeGenSharedCtxt_ :: DynFlags -> Bool -> CompCtxt -> Cg a -> Cg a
-- For contexts that we know by construction contain no statements
codeGenSharedCtxt_ dflags emit_global ctxt action
  = do { (a,stms) <- codeGenSharedCtxt dflags emit_global ctxt action
       ; unless (null stms) $
         fail "codeGenSharedCtxt_: non-empty statements!"
       ; return a
       }

genOrigin :: SrcLoc -> Cg ()
genOrigin loc = appendStmt [cstm|ORIGIN($string:(displayLoc (locOf loc))); |]
