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

module Opts where

import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle,
                  IOMode(..),
                  hClose,
                  openFile,
                  stderr)

import Text.PrettyPrint.HughesPJ

import System.Console.GetOpt
import System.Environment
import System.Exit (exitFailure)
import System.IO
import System.IO.Unsafe ( unsafePerformIO )

cMAX_STACK_ALLOC :: Int
cMAX_STACK_ALLOC = 32 * 1024

data DynFlag =
    InputFile String
  | OutputFile String
  | IncludePath FilePath
  | CSrcPathPosix FilePath
  | CSrcPathNative FilePath
  | Name String
  | CamlSyntax
  | Debug
  | DebugFold
  | StdoutDump
  | BoundsCheck

  | NoStaticEval

  | NativeMitigators
  | NoLUTHashing

  | ClosureConvert

  | Opt
  | Verbose
  | DumpVect
  | DumpTypes
  | DumpAst
  | DumpAstPretty

  -- Atomix paramters
  | DumpAutomaton
  | DumpDependencyGraphs
  | PrintPipeNames
  | PrintAtoms
  | PruneIncompleteStates
  | Optimism Int
  | FuseAggressively
  | CLikeNames
  | NoAtomThreads Int

  | Vectorize
  | AutoLUT
  | MaxLUTSize Integer -- ^ Max size of LUT, in bytes
  | StackAllocThreshold Int
  | Pipeline
  | AffinityMask Int
  | DummyThread
  | DumpToFile
  | DumpFold
  | DumpInline
  | DumpVectTypes
  | DumpPipeline
  | DumpLUT
  | DumpAutoLUT
  | DumpRange

  | NoExpFold
  | NoFold
  | NoElimMit
  | NoLUT

  | Timeout Integer -- In seconds

  | MockLUT -- just for debugging LUT

  | AtomixCodeGen

  deriving (Eq,Show)

type DynFlags = [DynFlag]

isDynFlagSet :: DynFlags -> DynFlag -> Bool
isDynFlagSet flags f = f `elem` flags

verbose :: MonadIO m => DynFlags -> Doc -> m () 
verbose dflags doc 
  | isDynFlagSet dflags Verbose 
  = liftIO $ do putStrLn (render doc) 
                hFlush stdout
  | otherwise = return ()


mAX_LUT_SIZE_DEFAULT :: Integer
mAX_LUT_SIZE_DEFAULT = 256*1024

maxLUTSize :: DynFlags -> Integer
maxLUTSize dflags =
    case [sz | MaxLUTSize sz <- dflags] of
      []     -> mAX_LUT_SIZE_DEFAULT
      sz : _ -> sz


dynFlagTimeout :: DynFlags -> Maybe Integer
dynFlagTimeout dflags = 
    case [sz | Timeout sz <- dflags] of
      []     -> Nothing
      sz : _ -> Just sz


dump :: MonadIO m
     => DynFlags   -- ^ Dynamic flags
     -> DynFlag    -- ^ Flag that must be set for dump to occur
     -> String     -- ^ File extension to use when dumping to a file (e.g. ".vec.wpl")
     -> Doc        -- ^ Doc to dump
     -> m ()
dump dflags dflag ext doc | isDynFlagSet dflags dflag = liftIO $
    withDumpHandle $ \h -> hPutStrLn h (render doc)
  where
    withDumpHandle :: (Handle -> IO ()) -> IO ()
    withDumpHandle m
       | isDynFlagSet dflags StdoutDump = m stdout >> putStrLn ""
       | otherwise
       = case [path | InputFile path <- dflags] of
            path : _ -> do h <- openFile (path ++ ext) WriteMode
                           m h `E.finally`  hClose h
            _        -> m stderr

dump _ _ _ _ = return ()


-- The actual options

options :: [OptDescr DynFlag]
options =
     [ Option ['o']     ["output"]      (ReqArg OutputFile  "FILE")           "output FILE"
     , Option ['i']     ["input"]       (ReqArg InputFile   "FILE")           "input FILE"
     , Option ['I']     []              (ReqArg IncludePath "PATH")           "include path PATH"
     , Option []        ["csrc-native"] (ReqArg CSrcPathNative "CSRC_NATIVE") "csrc-native path PATH"
     , Option []        ["csrc-posix"]  (ReqArg CSrcPathPosix  "CSRC_POSIX")  "csrc-posix path PATH"
     -- Create a unique name for go function to allow multiple functions to be called
     , Option ['n']     ["name"]        (ReqArg Name  "NAME")                 "go function name NAME"

     -- Boolean flags
     , Option []    ["caml-syntax"]      (NoArg CamlSyntax)    "Caml syntax"
     , Option ['d'] ["debug"]            (NoArg Debug)         "debug"
     , Option []    ["debug-fold"]       (NoArg DebugFold)     "debug-fold"
     , Option ['x'] ["optimize"]         (NoArg Opt)           "optimize"
     , Option ['v'] ["verbose"]          (NoArg Verbose)       "verbose"
     , Option ['a'] ["bounds-check"]     (NoArg BoundsCheck)   "bounds check"
     , Option []    ["stdout-dump"]      (NoArg StdoutDump)    "dump to stdout instead of files"
     , Option []    ["ddump-vect"]       (NoArg DumpVect)      "dump vectorization output"
     , Option []    ["ddump-types"]      (NoArg DumpTypes)     "dump a typechecked version of program"
     , Option []    ["ddump-vect-types"] (NoArg DumpVectTypes) "dump typechecked vectorized program"
     , Option []    ["ddump-ast"]        (NoArg DumpAst)       "dump the parsed AST"

     , Option []    ["ddump-automaton"]  (NoArg DumpAutomaton) "dump automaton for Atomix"
     , Option []    ["ddump-dependency-graphs"] (NoArg DumpDependencyGraphs) "dump atomix dependency graphs"
     , Option []    ["atomix-codegen"]   (NoArg AtomixCodeGen) "generate code a la Atomix"

     , Option []    ["print-pipe-names"]  (NoArg PrintPipeNames) "show names of pipes in Automaton"
     , Option []    ["print-atoms"]  (NoArg PrintAtoms)        "print atoms in automaton-dump"
     , Option []    ["prune-incomplete-states"]  (NoArg PruneIncompleteStates) "prune automaton states that terminate with data still in the pipeline"
     , Option []    ["optimism"]         (OptArg parseOptimism "INTEGER")      "pipeline optimism"
     , Option []    ["fuse-aggressively"] (NoArg FuseAggressively)             "fuse atoms at the cost of duplicating code"
     , Option []    ["c-like-names"]     (NoArg CLikeNames)                    "use same atom names as code generator"
     , Option []    ["no-atom-threads"]  (ReqArg parseNoAtomThreads "NO_THREADS")  "number of threads to be used for atom scheduling"

     , Option []    ["ddump-ast-pretty"] (NoArg DumpAstPretty) "dump the parsed AST (pretty-printed)"
     , Option []    ["vectorize"]        (NoArg Vectorize)     "vectorize program"
     , Option []    ["autolut"]          (NoArg AutoLUT)       "automatically convert function to use LUTs"
     , Option []    ["pipeline"]         (NoArg Pipeline)      "pipeline standalone computations"

     , Option []    ["closure-convert"]  (NoArg ClosureConvert) "generate code via closure conversion"


     , Option []    ["no-lut-hashing"]   (NoArg NoLUTHashing)  "do not hash lut generation"

     , Option []    ["dummy-thread"]     (NoArg DummyThread)   "generate dummy thread when pipelining"
     , Option []    ["ddump-to-file"]    (NoArg DumpToFile)    "dump to a file"
     , Option []    ["ddump-fold"]       (NoArg DumpFold)      "dump results of folding"
     , Option []    ["ddump-inline"]     (NoArg DumpInline)    "dump results of inlining"
     , Option []    ["ddump-pipeline"]   (NoArg DumpPipeline)  "dump results of pipelining"
     , Option []    ["ddump-lut"]        (NoArg DumpLUT)       "dump results of LUTting"
     , Option []    ["ddump-autolut"]    (NoArg DumpAutoLUT)   "dump results of auto-LUT"
     , Option []    ["ddump-range"]      (NoArg DumpRange)     "dump results of range analysis"


     , Option []    ["no-exp-fold"]      (NoArg NoExpFold)     "do not fold expressions"
     , Option []    ["no-static-eval"]   (NoArg NoStaticEval)  "do not statically evaluate expressions"

     , Option []    ["no-fold"]          (NoArg NoFold)        "do not fold computations and expressions"
     , Option []    ["no-lut"]           (NoArg NoLUT)         "do not construct LUTs"
     , Option []    ["no-elim-mit"]      (NoArg NoElimMit)     "do not optimize away mitigators"

     , Option []    ["mock-lut"]         (NoArg MockLUT)       "debugging help for LUTS (internal only)"

     , Option []    ["max-lut"]          (ReqArg parseMaxLUTOpt "SIZE")    "max lut size"
     , Option []    ["timeout"]          (ReqArg parseTimeout   "TIME")    "timeout (seconds)"

     , Option []    ["stack-threshold"]  (ReqArg parseStkThres  "SIZE")    "stack allocation threshold"
     , Option []    ["affinity-mask"]    (OptArg parseAffinityMask "MASK") "affinity mask"

     ]

usage :: String
usage = "Usage: wpl [OPTION...] files..."

parseOptimism :: Maybe String -> DynFlag
parseOptimism Nothing = Optimism defaultOptimism
parseOptimism (Just i) = Optimism (read i)

defaultOptimism :: Int
defaultOptimism = 0

parseAffinityMask :: Maybe String -> DynFlag
parseAffinityMask Nothing  = AffinityMask defaultAffinityMask
parseAffinityMask (Just i) = AffinityMask $ read i

parseStkThres :: String -> DynFlag
parseStkThres i = StackAllocThreshold (read i)

parseTimeout :: String -> DynFlag
parseTimeout i = Timeout (read i)

defaultMaxStkThreshold :: Int
defaultMaxStkThreshold = cMAX_STACK_ALLOC

defaultAffinityMask :: Int
defaultAffinityMask = 255

parseNoAtomThreads :: String -> DynFlag
parseNoAtomThreads s = NoAtomThreads (read s)

parseMaxLUTOpt :: String -> DynFlag
parseMaxLUTOpt s =
    case reads s of
      (n, "")  : _ -> MaxLUTSize n
      (n, "k") : _ -> MaxLUTSize (n*1024)
      (n, "K") : _ -> MaxLUTSize (n*1024)
      (n, "m") : _ -> MaxLUTSize (n*1024*1024)
      (n, "M") : _ -> MaxLUTSize (n*1024*1024)
      _            -> error $ "bad argument to --max-lut option: " ++ s

compilerOpts :: [String] -> IO (DynFlags, [String])
compilerOpts argv =
  case getOpt Permute options argv of
         (o,n,[]  ) -> return (o,n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))

getInFile :: DynFlags -> IO String
getInFile opts =
  case opts of
    [] -> ioError (userError ("No input file given\n" ++ usage))
    (InputFile infile : _) -> return infile
    (_ : opts') -> getInFile opts'

getOutFile :: DynFlags -> IO String
getOutFile opts =
  case opts of
    [] -> ioError (userError ("No output file given\n" ++ usage))
    (OutputFile outfile : _) -> return outfile
    (_ : opts') -> getOutFile opts'

getAffinityMask :: DynFlags -> Int
getAffinityMask opts =
  case opts of
    [] -> defaultAffinityMask
    (AffinityMask mask : _) -> mask
    (_ : opts') -> getAffinityMask opts'

getStkThreshold :: DynFlags -> Int
getStkThreshold opts =
  case opts of
    [] -> defaultMaxStkThreshold
    (StackAllocThreshold x : _) -> x
    (_ : opts') -> getStkThreshold opts'

getName :: DynFlags -> String
getName opts =
  case opts of
    [] -> ""
    (Name name : _) -> name
    (_ : opts') -> getName opts'

getOptimism :: DynFlags -> Int
getOptimism dfs =
  case dfs of
    [] -> defaultOptimism
    (Optimism optimism : _) -> optimism
    (_ : dfs) -> getOptimism dfs

getNoAtomThreads :: DynFlags -> Int
getNoAtomThreads opts = 
  case opts of
    [] -> 1
    (NoAtomThreads x : _) -> x
    (_ : opts') -> getNoAtomThreads opts'
