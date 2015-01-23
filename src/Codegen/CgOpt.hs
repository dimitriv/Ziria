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
import qualified Data.Loc
import Data.Monoid
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJ 
import Data.Maybe

import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO(..))

import Data.List ( nub )

import CgFun
import Outputable

------------------------------------------------------------------------------
-- | Computation Code Generation
------------------------------------------------------------------------------

compGenBind :: DynFlags
            -> GName CTy
            -> [GName (CallArg Ty CTy)]
            -> Comp
            -> CompFunGen
compGenBind dflags f params c1_with_locals args k = do
    let (locals, c1) = extractCMutVars c1_with_locals

    appName <- nextName $ "__" ++ (name f) ++ "_"
    let appId = appName

    -- The environment of locals
    let localEnv = [ (mutVar, [cexp|$id:(name nm_fresh)|])
                   | MutVar{..} <- locals
                   , let nm_fresh = mutVar { name = appId ++ name mutVar }
                   ]

    -- A freshened version of locals that uses the new ids!
    let locals' = map (\mv -> setMutVarName (appId ++ name (mutVar mv)) mv) locals

    let (eparams,cparams) = partitionParams params
    let (eargs, cargs)    = partitionCallArgs args

    (args_decls, args_stms, code_args) <- inNewBlock $
                                          mapM (codeGenExp dflags) eargs


    let eparamsEnv = zip eparams code_args

    let new_c1 = foldl (\xc (nm,ce) ->
                  cLet (compLoc c1) nm ce xc) c1 (zip cparams cargs)

    pushName appName

    c1info <- extendVarEnv (eparamsEnv ++ localEnv) $ do
              codeGenDeclGlobalGroups dflags (map mutVar locals') >>= appendDecls
              codeGenComp dflags new_c1 k

    (local_decls, local_stms) <- inNewBlock_ $
                                 extendVarEnv (eparamsEnv ++ localEnv) $
                                 codeGenGlobalInitsOnly dflags locals'
    appendDecls local_decls
    appendDecls args_decls

    return $ c1info { compGenInit = -- Wrong! codeDecls args_decls  `mappend`
                                    codeStmts args_stms   `mappend`
                                    codeStmts local_stms  `mappend`
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

mkUntil :: DynFlags
        -> Cg C.Exp
        -> Cg ()
        -> Comp
        -> CompInfo
        -> CompKont
        -> Cg CompInfo
mkUntil dflags mtest mfinal c cinfo k =
    codeGenComp dflags c (k { kontDone = doneKont })
  where
    doneKont = do
        ce_test <- mtest
        if ce_test
          then kontDone k
          else do mfinal
                  emitCode (compGenInit cinfo)
                  if canTick cinfo
                    then appendStmt  [cstm|goto $id:(tickNmOf (tickHdl cinfo));|]
                    else appendStmts [cstms|$id:globalWhatIs = SKIP;
                                            goto l_IMMEDIATE;|]

mkWhile :: DynFlags
        -> C.Stm       -- Initialization
        -> Exp         -- Test
        -> Comp -- Body
        -> C.Stm       -- Update
        -> CompInfo    -- Self
        -> CompKont    -- Outer continuation
        -> Cg CompInfo
-- Compile: while etest { cbody; mfinal }
mkWhile dflags minit etest cbody mfinal cinfo k
  = mkBranch dflags etest cbody k' cretunit k'' csp
  where
    csp = compLoc cbody
    k'  = k { kontDone = doneKont }
    k'' = k { kontDone = doneKont' k }
    cretunit = cReturn csp AutoInline $
               eVal csp TUnit VUnit

    -- MkComp (Return AutoInline (MkExp (EVal VUnit) csp TUnit)) csp cty
    doneKont = do appendStmt mfinal
                  emitCode (compGenInit cinfo)
                  if canTick cinfo
                  then appendStmt  [cstm|goto $id:(tickNmOf (tickHdl cinfo));|]
                  else appendStmts [cstms|$id:globalWhatIs = SKIP;
                                          goto l_IMMEDIATE;|]

    doneKont' k = do appendStmt minit
                     kontDone k


mkBranch :: DynFlags
         -> Exp
         -> Comp  -> CompKont  -- we give each branch his own continuation
         -> Comp  -> CompKont  -- we give each branch his own continuation
         -> Maybe SourcePos
         -> Cg CompInfo
mkBranch dflags e c1 k1 c2 k2 csp
  = do { branchName <- nextName ("__branch_" ++ (getLnNumInStr csp))
       ; let branch_id = branchName

       ; c1info <- codeGenCompTop dflags c1 k1
       ; c2info <- codeGenCompTop dflags c2 k2
       ; (edecls,estmts,ce) <- inNewBlock $ codeGenExp dflags e

       ; branch_var_name <- freshVar ("__branch_var_" ++ (getLnNumInStr csp))
       ; let branch_var = branch_var_name

       ; appendDecl =<< codeGenDeclVolGroup_init branch_var tint [cexp|0|]
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
         -> Maybe SourcePos
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

    is_struct_ptr <- isStructPtrType ty
    let yldty_is_pointer = isArrayTy ty || is_struct_ptr

    cgIO $ putStrLn "readCode! (after) "

    when yldty_is_pointer $
        -- Allocate a new array buffer
        appendDecl =<< codeGenDeclGroup yldTmpName ty

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
          -> Maybe SourcePos
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

    is_struct_ptr <- isStructPtrType ty
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
             , text "Location: " <+> (text $ show (compLoc comp))

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


    go (MkComp (Mitigate bty i1 i2) csp ()) = do
        -- Assume i1 `mod` i2 = 0
        mitName <- nextName ("__mit_" ++ (getLnNumInStr csp))
        let prefix = mitName
        let ih = inHdl k
        let yh = yieldHdl k
        let mit_st = prefix ++ "_mit_state"
        let buf = prefix ++ "_mit_buff"

        appendStmt [cstm|ORIGIN($string:(show csp)); |]

        if (i1 >= i2)
          then do { let d = i1 `div` i2

                    -- declare the state of the mitigator
                  ; appendDecl =<<
                       codeGenDeclVolGroup_init mit_st tint [cexp|$int:d|]

                  ; let arrty = TArray (Literal i1) bty
                        leninfo = if i2 == 1 then LISingleton else LILength i2

                  ; appendLabeledBlock (tickNmOf prefix) $ do
                        if [cexp| $id:mit_st >= $int:d |] then kontConsume k
                        else do
                           cres <- codeGenArrRead dflags
                                                  arrty
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

                        cres <- codeGenArrRead
                                      dflags
                                      arrty [cexp|$id:(inValOf ih)|]
                                            (AIdxStatic 0)
                                            leninfo

                        appendStmt [cstm|$id:(yldValOf yh) = $cres;|]
                        appendStmt [cstm|$id:mit_st = 1;|]
                        kontYield k

                  ; return (mkCompInfo prefix True) -- can tick

                  }

          else do { let d = i2 `div` i1

                    -- declare the state of the mitigator
                  ; appendDecl =<<
                       codeGenDeclVolGroup_init mit_st tint [cexp|0|]
                  ; appendDecl =<<
                       codeGenDeclVolGroup buf (TArray (Literal i2) bty)

                    -- trivial tick()
                  ; appendLabeledBlock (tickNmOf prefix) $ do
                           kontConsume k

                  ; let arrty   = TArray (Literal i2) bty
                        leninfo = if i1 == 1 then LISingleton else LILength i1

                  ; appendLabeledBlock (processNmOf prefix) $ do
                           -- buff[mit_st,leninfo] := in_val
                           codeGenArrWrite dflags arrty [cexp|$id:buf|]
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

                  ; return (mkCompInfo prefix False) -- cannot tick

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
            TArrow [invalty] _ = nameTyp nm
        let invalname = MkName { name    = inValOf ih
                               , uniqId  = MkUniq $ inValOf ih -- yikes
                               , nameTyp = invalty
                               , nameLoc = invalloc }
            invalarg  = eVar invalloc invalname

        let ecall     = eCall invalloc nm [invalarg]

        appendStmt [cstm|ORIGIN($string:(show csp)); |]

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

        appendDecl =<< codeGenDeclVolGroup_init stateVar tint [cexp|0|]
        appendDecl =<< codeGenDeclGroup expVar (ctExp e)


        appendStmt [cstm|ORIGIN($string:(show csp)); |]
        -- Must generate emit_process since runtime code will use name (even
        -- if it never calls the function) when "emit" is the toplevel
        -- program
        appendLabeledBlock (tickNmOf prefix) $ do
            if [cexp|$id:stateVar == 0|]
              then do ce <- codeGenExp dflags e
                      assignByVal (ctExp e) (ctExp e) [cexp|$id:expVar|] ce
              else return ()

            if [cexp|$id:stateVar < $int:n|]
              then do ce <- codeGenArrRead dflags
                               (ctExp e) [cexp|$id:expVar|]
                               (AIdxCExp [cexp|$id:stateVar|]) LISingleton

                      assignByVal yldTy yldTy [cexp|$id:(yldValOf yh)|] ce
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

        return (mkCompInfo prefix True) {compGenInit = codeStmt [cstm|$id:stateVar = 0;|]}
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


        appendStmt [cstm|ORIGIN($string:(show csp)); |]

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

        appendStmt [cstm|ORIGIN($string:(show csp)); |]

        if isArrayTy ty then
          -- Allocate a new array buffer
          do appendDecl =<< codeGenDeclGroup yldTmpName ty
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
        appendDecl =<< codeGenDeclVolGroup_init stateVar tint [cexp|FALSE|]

        mapM_ appendStmt [cstms| $comment:("/* " ++ show csp ++ " */") |]

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

        appendDecl =<< codeGenDeclVolGroup_init branch_var tint [cexp|0|]

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

                new_dh <- freshName ("__dv_tmp_"  ++ (getLnNumInStr csp)) vTy
                let new_dhval = doneValOf $ name new_dh
                appendDecl =<<
                    -- Note [Take Optimization]
                    -- If the component is a Take1 then we can simply make
                    -- sure that we return the address of the in value, and
                    -- hence we don't have to declare storage for doneVal.
                    if is_take (unComp c1) && isArrayTy vTy then
                       return [cdecl| $ty:(codeGenTyAlg vTy) $id:new_dhval;|]
                    else
                       codeGenDeclGroup new_dhval vTy


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
                return $ c1info : rest_info

            go branch_var num_branches (c1,c1Name) [] = do
                pushName c1Name
                c1info <- codeGenCompTop dflags c1 k
                return [c1info]

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
        appendStmt [cstm|ORIGIN($string:(show csp)); |]

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

        b <- isStructPtrType yTy

        -- Yield never allocates, just points to the correct places
        -- But NB we initialize to dummy values because the upstream component
        -- may never really yield anything and hence we may leave the temporary
        -- variable uninitialized, which confuses the C compiler and gives weird
        -- error messages about uninitialized intermediate __yv_tmp values.
        -- Example:   do { error "foo" } >>> bar
        let yTy_c = codeGenTyAlg yTy
        yv_init <- (codeGenTy_val yTy :: Cg C.Initializer)
        let foo = yv_init
        let ydecl | (b && not (isArrayTy yTy)) -- Not already a pointer
                  = [cdecl| $ty:yTy_c * $id:new_yhval = NULL; |]
                  | otherwise
                  = [cdecl| $ty:yTy_c $id:new_yhval = $init:yv_init; |]

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
        unusedName <- freshName ("__seq_unused_" ++ (getLnNumInStr csp)) dty
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
            if isArrayTy inty then
               appendStmt [cstm|$id:(doneValOf dh) = $id:(inValOf ih);|]
            else
               assignByVal inty inty donevalexp invalofexp

            -- New:
            appendStmt [cstm|$id:(doneValOf dh) = $id:(inValOf ih);|]

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

        appendDecl =<< codeGenDeclVolGroup_init stateVar tint [cexp|0|]

        appendLabeledBlock (tickNmOf prefix) $
            kontConsume k

        appendLabeledBlock (processNmOf prefix) $ do
            codeGenArrWrite dflags aTy [cexp|$id:(doneValOf dh)|]
                                       (AIdxCExp [cexp|$id:stateVar|]) LISingleton [cexp|$id:(inValOf ih)|]

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
            assignByVal (ctExp e) (ctExp e) [cexp|$id:(doneValOf dh)|] ce
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
        new_dh <- freshName ("__dv_tmp_"  ++ (getLnNumInStr csp)) vTy
        let new_dhval = doneValOf $ name new_dh
        codeGenDeclGroup new_dhval vTy >>= appendDecl
        codeGenFixpoint (mkRepeat dflags c1) (k { doneHdl = name new_dh})

    go (MkComp (Until e c1) csp ()) =
        codeGenFixpoint (mkUntil dflags (codeGenExp dflags e) (return ()) c1) k


    go (MkComp (VectComp _ c) _ _) = go c  -- Just ignore vectorization annotation if it has survived so far.

    go (MkComp (While e c1) csp ()) =
        let emptyStm = [cstm|UNIT;|]
        in codeGenFixpoint (mkWhile dflags emptyStm e c1 emptyStm) k


    go cb@(MkComp (Times _ui estart elen nm c1) csp ()) = do
        -- @nm@ scopes over @e@ and @c1@, so we need to gensym here to make sure
        -- it maps to a unique C identifier and then bring it into scope below
        -- when we generate code for @e@ and @c1@. We also have to make sure we
        -- initialize the loop counter as part of the component's
        -- initialization.

        nm' <- freshName ("__times_" ++ name nm ++ getLnNumInStr csp) (nameTyp nm)
        appendDecl [cdecl|$ty:(codeGenTy (nameTyp nm)) $id:(name nm') = 0;|]

        cestart <- codeGenExp dflags estart

        extendVarEnv [(nm,[cexp|$id:(name nm')|])] $ do

        let efinal = eBinOp csp Add elen estart

        let mtest  = eBinOp csp Lt (eVar csp nm) efinal
            mfinal = [cstm|++$id:(name nm');  |]
            minit  = [cstm|$id:(name nm') = $cestart;|]

        cinfo <- codeGenFixpoint (mkWhile dflags minit mtest c1 mfinal) k

        let minits = compGenInit cinfo `mappend` codeStmt minit
        return cinfo { compGenInit = minits }

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

           ; x_decl <- codeGenDeclGroup x_cname ty

           ; if emit_global then
                appendTopDecls (x_decl : e_decls)
             else
                appendDecls (x_decl : e_decls)

           ; (a,stms) <- extendVarEnv [(x, [cexp|$id:x_cname|])] $
                         do { -- cgIO $ putStrLn "before action."
                            ; r <- go ctxt action
                              -- cgIO $ putStrLn "after action."
                            ; return r
                            }

           ; (_,asgn_stmts) <- inNewBlock_ $
                               assignByVal ty ty [cexp|$id:x_cname|] ce

           ; return (a,e_stmts ++ asgn_stmts ++ stms)
           }

    -- CL
    -- TODO: Should we do something with _ty here?
    go (CLetERef csp x (Just e) ctxt) action
      = do { (e_decls,e_stmts,ce) <- inNewBlock $ codeGenExp dflags e
           ; let ty = ctExp e
           ; x_name <- freshVar ((name x) ++ "_" ++ (getLnNumInStr csp))
           ; let x_cname = x_name

           ; x_decl <- codeGenDeclGroup x_cname ty

           ; if emit_global then
                appendTopDecls (x_decl : e_decls)
             else
                appendDecls (x_decl : e_decls)

           ; (a,stms) <- extendVarEnv [(x, [cexp|$id:x_cname|])] $
                         do { -- cgIO $ putStrLn "before action."
                            ; r <- go ctxt action
                              -- cgIO $ putStrLn "after action."
                            ; return r
                            }

           ; (_,asgn_stmts) <- inNewBlock_ $
                               assignByVal ty ty [cexp|$id:x_cname|] ce

           ; return (a,e_stmts ++ asgn_stmts ++ stms)
           }
    go (CLetERef csp x Nothing ctxt) action
      = do { x_name <- freshVar ((name x) ++ "_" ++ (getLnNumInStr csp))
           ; let x_cname = x_name

           ; x_decl <- codeGenDeclGroup x_cname (nameTyp x)

           ; if emit_global then
                appendTopDecls [x_decl]
             else
                appendDecls [x_decl]

           ; res <- extendVarEnv [(x, [cexp|$id:x_cname|])] $
                    do { -- cgIO $ putStrLn "before action."
                        ; r <- go ctxt action
                         -- cgIO $ putStrLn "after action."
                        ; return r
                       }
           ; return res
           }
    --
    go (CLetFunC csp f params c1 ctxt) action
      = extendFunEnv f (compGenBind dflags f params c1) $
        go ctxt action

    go (CLetHeader csp (MkFun (MkFunExternal nm ps retTy) _ _) ctxt) action =
        codeGenLetExternalFun retTy
      where

        codeGenLetExternalFun (TArray _ _ ) = do

           let retN = "__retf_" ++ name nm

           let extNm = "__ext_" ++ name nm
           let extF = toName ("__ext_" ++ name nm) Nothing (nameTyp nm)

           -- We cannot return an array declared within a function.  Instead we
           -- declare a global variable, store the return in it, and use that
           -- pointer as a return value
           let retN_name = toName retN Nothing retTy

           cparams <- codeGenParams (retN_name : ps)
           appendTopDecls [ [cdecl|void $id:(extNm)($params:cparams);|] ]

           extendExpFunEnv nm (extF,[]) $ go ctxt action

        codeGenLetExternalFun _ = do
            cparams <- codeGenParams ps
            let extNm = "__ext_" ++ name nm
            let extF = toName ("__ext_" ++ name nm) Nothing (nameTyp nm)

            appendTopDecls [ [cdecl|$ty:(codeGenTy retTy)
                                           $id:(extNm)($params:cparams);|] ]
            extendExpFunEnv nm (extF,[]) $ go ctxt action

    go (CLetHeader csp fdef@(MkFun (MkFunDefined {}) _ _) ctxt) action =
        -- defined CgFun
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
