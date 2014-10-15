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

module Main where

import Control.Exception
import Control.Monad (when, unless, foldM, forM)
import Data.Maybe
import Data.Monoid
import System.Console.GetOpt
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Text.Parsec
import Text.PrettyPrint.Mainland
import Text.Show.Pretty (dumpStr)
import qualified Data.Map          as M
import qualified Language.C.Syntax as C

-- TODO: There are a lot of redundant imports here. Enable -Wall in Main.
import AstComp
import AstExpr
import AstLabelled   as Labelled
import AstUnlabelled as Unlaballed
import CtComp (ctComp)
import Opts
import PassFold
import PpComp
import Rename
import TcComp
import TcErrors ( ErrCtx (..) )
import TcExpr
import TcMonad
import PassPipeline
import qualified AstCombinator
import qualified BlinkParseComp as NewParser
import qualified GenSym         as GS
import qualified Outputable -- Qualified so that we don't clash with Mainland

import CgOpt 


import AutoLUT
import CardinalityAnalysis
import CgHeader
import CgMonad
import CgOpt

import CgProgram ( codeGenProgram )

import qualified PassPipeline as PP


-- import Vectorize
import Orphans



{-
data CompiledProgram = CompiledProgram (Comp CTy Ty) [C.Definition] FilePath
-}

pprProgInfo :: Comp -> Doc
pprProgInfo prog =
    (text . show) prog </>
    line <>
    text "type:" </>
    (text . show . compInfo) prog

outputProgram :: Comp -> FilePath -> IO ()
outputProgram c fn = do
    outHdl <- openFile fn WriteMode
    hPutDoc outHdl $ pprProgInfo c
    hClose outHdl

{-
outputCompiledProgram :: CompiledProgram -> IO ()
outputCompiledProgram (CompiledProgram sc cc fn) = do
    outHdl <- openFile fn WriteMode
    hPutStrLn outHdl "/*"
    hPutDoc outHdl $ pprProgInfo sc
    hPutStrLn outHdl "*/"
    hPutStr outHdl cHeader
    hPutStr outHdl $ show $ ppr cc
    hClose outHdl
-}

main :: IO ()
main = failOnException $ do
    hSetBuffering stdout NoBuffering

    -- putStrLn "pre-command line parsing ..."

    args <- getArgs
    (dflags, _) <- compilerOpts args

    inFile  <- getInFile dflags
    outFile <- getOutFile dflags
    input <- readFile inFile

    -- putStrLn "command line parsed ..."
    prog <-
          failOnError $
          do { let st = ()
                   pm = runParserT NewParser.parseProgram st inFile input
             ; NewParser.runParseM pm [] }

    -- pretty-show's `dumpDoc` generates a `Text.PrettyPrint.HughesPJ.Doc`
    -- rather than `Text.PrettyPrint.Mainland.Doc`, so we generate a flat
    -- string instead
    dump dflags DumpAst ".ast.dump" $ (text . dumpStr) prog
    dump dflags DumpAstPretty ".ast.pretty.dump" $ (text . show) prog

    rensym <- GS.initGenSym (getName dflags)
    prog_renamed <- runRenM rensym (rename prog)
    sym <- GS.initGenSym (getName dflags)

    -- putStrLn $ "renamed ... " ++ show prog_renamed

    let cenv     = mkCEnv []
    let tdef_env = mkTyDefEnv primComplexStructs
    let varenv   = mkEnv []

    -- Maybe we should combine the two calls to the type checker?
    (globals', unifiers0)
       <- failOnError $
          runTcM (tyCheckTopDecls (globals prog_renamed))
                 tdef_env
                 varenv
                 cenv
                 sym
                 GlobalDefs
                 emptyTcMState


    -- putStrLn "typechecked dels ..."

    let decl_env = envOfDecls globals'

    (c', unifiers1)
       <- failOnError $
          runTcM (tyCheckTopComp (progComp prog_renamed))
                 tdef_env
                 decl_env
                 cenv
                 sym
                 GlobalDefs
                 unifiers0

    let in_ty  =  inTyOfCTyBase (ctComp c')
        yld_ty = yldTyOfCTyBase (ctComp c')

    when (isDynFlagSet dflags Debug) $ outputProgram c' outFile

    when (not (isDynFlagSet dflags Debug)) $ do
    dump dflags DumpTypes ".type.dump" $ (text . show) (ppCompTyped c')

    -- putStrLn "typechecked program ..."

    -- First let us run some small-scale optimizations
    folded <- runFoldPhase dflags sym 1 c'

    return ()
{-
    -- putStrLn $ "run the fold phase ..." ++ show folded

    -- putStrLn $ "proceeding with vectorization ...."

    cands <- runVectorizePhase dflags sym tdef_env decl_env cenv unifiers1 $
             folded

    -- The vectorizer returns a list of candidate
    -- vectorizations. But I think that if it is empty then this
    -- indicates vectorization failure. Moreover I think it can never
    -- return more than one candidate.  However the code is prepared
    -- for more than one candidates (and we might need to re-enable it
    -- for experimentation. Hence I am adding some assertions below to
    -- see when this happens (if at all ...)
    check_vect_cands cands

    -- Open files!
    let fnames = outFile : ["v"++show i++"_"++outFile | i <- [0..]]
    let ccand_names = zip cands fnames

    -- putStrLn "post vectorized (before 2nd round of fold) ..."

    -- Fold, inline, and pipeline
    icands <- mapM (runPostVectorizePhases dflags sym) ccand_names


    -- putStrLn "post vectorized ..."

    -- Generate code
    -- Use module name to avoid generating the same name in different modules
    sym <- GS.initGenSym (getName dflags)

    let compile_threads (sc,ctx,tid_cs,bufTys,fn)
          = do { defs <- failOnError $
                         evalCg sym (getStkThreshold dflags) $
                         codeGenProgram dflags globals'
                                        ctx tid_cs bufTys (in_ty,yld_ty)
               ; return $ CompiledProgram sc defs fn }
    code_names <- forM icands compile_threads

    -- putStrLn "post code generation ..."

    mapM_ outputCompiledProgram code_names
-}
  where
{-
    check_vect_cands cands
      = case cands of
         [c] -> return ()
         []  -> failWithError "Vect failure: non-sensical annotations?"
         _   -> failWithError "Vect failure: too many candidates?"
-}

    runFoldPhase :: DynFlags -> GS.Sym -> Int -> Comp -> IO Comp
    -- Fold Phase
    runFoldPhase dflags sym i c
      | isDynFlagSet dflags Opt
      = do { c' <- runFold dflags sym c
           ; dump dflags DumpFold (".fold-phase" ++ show i ++ ".dump")
                                 ((text . show . Outputable.ppr) c')
           ; return c' }
      | otherwise
      = return c

{-
    runAutoLUTPhase :: DynFlags -> GS.Sym -> Comp CTy Ty -> IO (Comp CTy Ty)
    -- AutoLUTPhase
    runAutoLUTPhase dflags sym c
      | isDynFlagSet dflags AutoLUT
      = do { verbose dflags $ text "Auto-LUTting (start)"
           ; c' <- runAutoLUT dflags sym c
           ; dump dflags DumpAutoLUT (".autolut.dump")
                                     ((text . show . Outputable.ppr) c')
           ; verbose dflags $ text "Auto-LUTting (end)"
           ; return c' }
      | otherwise
      = return c

    runVectorizePhase :: DynFlags
                      -> GS.Sym
                      -> TyDefEnv
                      -> Env
                      -> CEnv
                      -> TcMState
                      -> Comp CTy Ty
                      -> IO [Comp CTy Ty]
    -- Vectorize Phase
    runVectorizePhase dflags sym tdef_env decl_env cenv st c
      | isDynFlagSet dflags Vectorize
      = runDebugVecM dflags c tdef_env decl_env cenv sym st
      | otherwise
      = return [c]

    runPipelinePhase :: DynFlags -> GS.Sym -> Comp CTy Ty
                     -> IO PP.PipelineRetPkg
    -- Pipeline Phase
    runPipelinePhase dflags _ c
      | isDynFlagSet dflags Pipeline
      = PP.runPipeLine (isDynFlagSet dflags DumpPipeline) c
      | otherwise
      = return $ PP.MkPipelineRetPkg Hole [("",c)] []

    runPostVectorizePhases :: DynFlags
        -> GS.Sym
        -> (Comp CTy Ty, FilePath)
        -> IO (Comp CTy Ty, CompCtxt, [(String, Comp CTy Ty)], [Ty], FilePath)
    runPostVectorizePhases dflags sym (c,fn) = do
        verbose dflags $
           text "Result in file:" <+> text fn
        dump dflags DumpVect ".vec.dump" $
           text "Result:" </> (text . show . ppCompTypedVect) c
        dump dflags DumpVectTypes ".vec-types.dump" $
           text "Result (with types):" </> (text . show . ppCompTyped) c

        -- Second round of folding
        fc <- runFoldPhase dflags sym 2 c

        lc <- runAutoLUTPhase dflags sym fc

        PP.MkPipelineRetPkg { PP.context = comp_ctxt
                            , PP.threads = comp_threads
                            , PP.buf_tys = tys }
          <- runPipelinePhase dflags sym lc
        return (lc, comp_ctxt, comp_threads,tys,fn)
-}

failOnError :: Show a => IO (Either a b) -> IO b
failOnError m = do
    res <- m
    case res of
      Left err -> failWithError err
      Right x -> return x

failOnException :: IO () -> IO ()
failOnException m =
    m `catch` \(e :: SomeException) -> do hFlush stdout
                                          hPrint stderr e
                                          exitFailure

failWithError :: Show a => a -> IO b
failWithError err
  = do hFlush stdout
       hPrint stderr err
       exitFailure
