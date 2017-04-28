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
import Text.PrettyPrint.HughesPJ
import qualified Text.PrettyPrint.Mainland as GMPretty
-- import Text.Show.Pretty (dumpStr)
import qualified Language.C.Syntax as C

import System.Timeout 
import Data.Time.Clock

import AstComp
import AstExpr

import CtComp (ctComp)
import Opts
import PassFold
import PpComp

import TcMonad
import Typecheck      ( tyCheckProg              )

import qualified GenSym         as GS
import qualified Outputable

import AutoLUT

import Vectorize    ( initVectorizer, runVectorizer  )

import CgProgram    ( codeGenProgram )
import CgMonad      ( evalCg         )
import CgHeader     ( cHeader        )

import qualified PassPipeline as PP

import qualified Language.Ziria.Parser as P

import TcRename () 

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
    hPutStr outHdl $ GMPretty.pretty 80 (GMPretty.ppr cc)
    hClose outHdl


withTimeout :: DynFlags -> IO a -> IO a 
withTimeout dfs action = 
   case dynFlagTimeout dfs of 
     Nothing -> action
     Just t  -> do
       r <- timeout (fromIntegral (t * 1000000)) action
       case r of Nothing -> error "Timout exceeded!" 
                 Just x  -> return x

timedPhase :: DynFlags -> String -> IO a -> IO a
timedPhase _dfs pname m = do 
  cur <- getCurrentTime 
  r <- m
  fin <- getCurrentTime
  let d = diffUTCTime fin cur
  putStrLn $ "Phase: " ++ pname ++  "(" ++ show d ++ ")"
  return r


main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering

  args <- getArgs
  (dflags, _) <- compilerOpts args

  withTimeout dflags $ failOnException $ do

    inFile  <- getInFile dflags
    outFile <- getOutFile dflags
    prog    <- if CamlSyntax `elem` dflags
               then P.camlParseProgramFromFile inFile
               else P.parseProgramFromFile inFile

    dump dflags DumpAst ".ast.dump" $ (text . show) prog
    -- Disabling Pretty for now
    dump dflags DumpAstPretty ".ast.dump" $ (text . show) prog

    sym <- GS.initGenSym (getName dflags)
    

    (MkProg c', _unifier) <- timedPhase dflags "tyCheckProg" $
                             failOnError $ runTcM' (tyCheckProg prog) sym
    let in_ty  =  inTyOfCTy (ctComp c')
        yld_ty = yldTyOfCTy (ctComp c')

    when (isDynFlagSet dflags Debug) $ outputProgram c' (outFile ++ ".debug")

    dump dflags DumpTypes ".type.dump" $ (text . show) (ppCompTyped ctComp c')

    -- First let us run some small-scale optimizations
    folded <- timedPhase dflags "runFoldPhase" $ 
              runFoldPhase dflags sym 1 c'


    initVectorizer

    (cand, cands) <- timedPhase dflags "runVectorizePhase" $ 
                     runVectorizePhase dflags sym folded

    -- Filenames
    let fnames = ["v"++show i++"_"++outFile | (i::Int) <- [0..]]
    let ccand_names = zip (cand:cands) (outFile : fnames)

    -- Fold, inline, and pipeline
    icands <- mapM (runPostVectorizePhases dflags sym) ccand_names

    -- Right before code generation let us type check everything
    when (isDynFlagSet dflags Debug) $ 
       timedPhase dflags "ctComp" $ 
       do { verbose dflags $ text "Passing through ctCtomp"
          ; mapM_ (\c -> seq (ctComp c) (return ())) (cand:cands)
          }

    -- Generate code
    -- Use module name to avoid generating the same name in different modules
    cg_sym <- GS.initGenSym (getName dflags)

    let compile_threads (sc,ctx,tid_cs,bufTys,fn)
          = do { defs <- failOnError $
                         evalCg cg_sym (getStkThreshold dflags) $
                         codeGenProgram dflags ctx tid_cs bufTys (in_ty,yld_ty)
               ; return $ CompiledProgram sc defs fn }
    code_names <- timedPhase dflags "codeGenProgram" $ 
                  forM icands compile_threads

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
                , ppCompTypedVect ctComp c ]
        dump dflags DumpVectTypes ".vec-types.dump" $
           vcat [ text "Result (with types):" 
                , ppCompTyped ctComp c ]

        -- Second round of folding
        fc <- timedPhase dflags "runFoldPhase (2nd round)" $ 
              runFoldPhase dflags sym 2 c

        lc <- timedPhase dflags "runAutoLUTPhase" $ 
              runAutoLUTPhase dflags sym fc

        PP.MkPipelineRetPkg { PP.context = comp_ctxt
                            , PP.threads = comp_threads
                            , PP.buf_tys = tys }
          <- timedPhase dflags "runPipelinePhase" $ 
             runPipelinePhase dflags sym lc
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
