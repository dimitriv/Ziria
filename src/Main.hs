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

import Control.Exception
import Control.Monad (when, forM)
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Text.Parsec (runParserT)
import Text.PrettyPrint.HughesPJ
import qualified Text.PrettyPrint.Mainland as GMPretty
import Text.Show.Pretty (dumpStr)
import qualified Language.C.Syntax as C

import System.Timeout 

import AstComp
import AstExpr

import CtComp (ctComp)
import Opts
import PassFold
import PpComp

import TcMonad
import Typecheck      ( tyCheckProg              )

import BlinkParseComp ( parseProgram             )
import BlinkParseM    ( mkZiriaStream, runParseM )

import qualified GenSym         as GS
import qualified Outputable

import AutoLUT

import qualified Analysis.RangeAnal as RA

import Vectorize    ( initVectorizer, runVectorizer  )

import CgProgram    ( codeGenProgram )
import CgMonad      ( evalCg         )
import CgHeader     ( cHeader        )

import qualified PassPipeline as PP


data CompiledProgram
  = CompiledProgram Comp [C.Definition] FilePath

pprProgInfo :: Comp -> Doc
pprProgInfo prog =
  vcat [ (text . show) prog
       , text "" 
       , text "type:"
       , (text . show . ctComp) prog ]

outputProgram :: Comp -> FilePath -> IO ()
outputProgram c fn = do
    outHdl <- openFile fn WriteMode
    hPutStrLn outHdl $ render $ pprProgInfo c
    hClose outHdl

outputCompiledProgram :: CompiledProgram -> IO ()
outputCompiledProgram (CompiledProgram sc cc fn) = do
    outHdl <- openFile fn WriteMode
    hPutStrLn outHdl "/*"
    hPutStrLn outHdl $ render $ pprProgInfo sc
    hPutStrLn outHdl "*/"
    hPutStr outHdl cHeader
    hPutStr outHdl $ show (GMPretty.ppr cc)
    hClose outHdl


withTimeout :: DynFlags -> IO a -> IO a 
withTimeout dfs action = 
   case dynFlagTimeout dfs of 
     Nothing -> action
     Just t  -> do
       r <- timeout (fromIntegral (t * 1000000)) action
       case r of Nothing -> error "Timout exceeded!" 
                 Just x  -> return x

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering

  args <- getArgs
  (dflags, _) <- compilerOpts args

  withTimeout dflags $ failOnException $ do

    inFile  <- getInFile dflags
    outFile <- getOutFile dflags
    input   <- readFile inFile

    



    prog <-
          failOnError $
          do { let state  = ()
                   stream = mkZiriaStream input
                   parser = runParserT parseProgram state
                                                    inFile
                                                    stream
             ; runParseM parser []
             }

    -- pretty-show's `dumpDoc` generates a `Text.PrettyPrint.HughesPJ.Doc`
    -- rather than `Text.PrettyPrint.Mainland.Doc`, so we generate a flat
    -- string instead
    dump dflags DumpAst ".ast.dump" $ (text . dumpStr) prog
    dump dflags DumpAstPretty ".ast.pretty.dump" $ (text . show) prog

    sym <- GS.initGenSym (getName dflags)
    
    verbose dflags $ text "tyCheckProg .."

    (MkProg c', _unifier) <- failOnError $ runTcM' (tyCheckProg prog) sym
    let in_ty  =  inTyOfCTy (ctComp c')
        yld_ty = yldTyOfCTy (ctComp c')

    when (isDynFlagSet dflags Debug) $ outputProgram c' (outFile ++ ".debug")

    dump dflags DumpTypes ".type.dump" $ (text . show) (ppCompTyped c')

    verbose dflags $ text "runFoldPhase .."

    -- First let us run some small-scale optimizations
    folded <- runFoldPhase dflags sym 1 c'


    verbose dflags $ text "runVectorizePhase .."

    initVectorizer

    (cand, cands) <- runVectorizePhase dflags sym folded


    when (isDynFlagSet dflags DumpRange) $ RA.debugRngAnal cand


    -- Filenames
    let fnames = ["v"++show i++"_"++outFile | (i::Int) <- [0..]]
    let ccand_names = zip (cand:cands) (outFile : fnames)

    verbose dflags $ text "runPostVectorizePhases .." 
    -- Fold, inline, and pipeline
    icands <- mapM (runPostVectorizePhases dflags sym) ccand_names

    verbose dflags $ text "ctComp before code generation .."

    -- Right before code generation let us type check everything
    when (isDynFlagSet dflags Debug) $ 
       do { verbose dflags $ text "Passing through ctCtomp"
          ; mapM_ (\c -> seq (ctComp c) (return ())) (cand:cands)
          }

    verbose dflags $ text "codeGenProgram .."    

    -- Generate code
    -- Use module name to avoid generating the same name in different modules
    cg_sym <- GS.initGenSym (getName dflags)

    let compile_threads (sc,ctx,tid_cs,bufTys,fn)
          = do { defs <- failOnError $
                         evalCg cg_sym (getStkThreshold dflags) $
                         codeGenProgram dflags ctx tid_cs bufTys (in_ty,yld_ty)
               ; return $ CompiledProgram sc defs fn }
    code_names <- forM icands compile_threads

    verbose dflags $ text "outputCompiledProgram .."

    mapM_ outputCompiledProgram code_names

  where

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


    runAutoLUTPhase :: DynFlags -> GS.Sym -> Comp -> IO Comp
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


    -- Vectorize Phase
    runVectorizePhase :: DynFlags -> GS.Sym -> Comp -> IO (Comp,[Comp])
    runVectorizePhase dflags sym c
      | isDynFlagSet dflags Vectorize
      = Vectorize.runVectorizer dflags sym c
      | otherwise
      = return (c,[])

    runPipelinePhase :: DynFlags -> GS.Sym -> Comp
                     -> IO PP.PipelineRetPkg
    -- Pipeline Phase
    runPipelinePhase dflags _ c
      | isDynFlagSet dflags Pipeline
      = PP.runPipeLine (isDynFlagSet dflags DumpPipeline) c
      | otherwise
      = return $ PP.MkPipelineRetPkg Hole [("",c)] []

    runPostVectorizePhases :: DynFlags
        -> GS.Sym
        -> (Comp, FilePath)
        -> IO (Comp, CompCtxt, [(String, Comp)], [Ty], FilePath)
    runPostVectorizePhases dflags sym (c,fn) = do
        verbose dflags $
           text "Result in file:" <+> text fn
        dump dflags DumpVect ".vec.dump" $
           vcat [ text "Result:"
                , ppCompTypedVect c ]
        dump dflags DumpVectTypes ".vec-types.dump" $
           vcat [ text "Result (with types):" 
                , ppCompTyped c ]

        -- Second round of folding
        fc <- runFoldPhase dflags sym 2 c

        lc <- runAutoLUTPhase dflags sym fc

        PP.MkPipelineRetPkg { PP.context = comp_ctxt
                            , PP.threads = comp_threads
                            , PP.buf_tys = tys }
          <- runPipelinePhase dflags sym lc
        return (lc, comp_ctxt, comp_threads,tys,fn)


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
