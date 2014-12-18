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
import Text.PrettyPrint.Mainland

import System.Console.GetOpt
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Text.Parsec

import CgMonad ( cMAX_STACK_ALLOC )

data DynFlag =
    InputFile String
  | OutputFile String
  | IncludePath FilePath
  | CSrcPathPosix FilePath
  | CSrcPathNative FilePath
  | Name String
  | Debug
  | DebugFold
  | StdoutDump
  | BoundsCheck

  | NativeMitigators
  | NoLUTHashing

  | Opt
  | Verbose
  | DumpVect
  | DumpTypes
  | DumpAst
  | DumpAstPretty
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

  | NoExpFold
  | NoFold

  | NoLUT

  | MockLUT -- just for debugging LUT

  deriving (Eq,Show)

type DynFlags = [DynFlag]

isDynFlagSet :: DynFlags -> DynFlag -> Bool
isDynFlagSet flags f = f `elem` flags

verbose :: MonadIO m => DynFlags -> Doc -> m ()
verbose dflags doc | isDynFlagSet dflags Verbose = liftIO $ putDoc $ doc <> line
                   | otherwise                   = return ()


mAX_LUT_SIZE_DEFAULT :: Integer
mAX_LUT_SIZE_DEFAULT = 128*1024

maxLUTSize :: DynFlags -> Integer
maxLUTSize dflags =
    case [sz | MaxLUTSize sz <- dflags] of
      []     -> mAX_LUT_SIZE_DEFAULT
      sz : _ -> sz

dump :: MonadIO m
     => DynFlags   -- ^ Dynamic flags
     -> DynFlag    -- ^ Flag that must be set for dump to occur
     -> String     -- ^ File extension to use when dumping to a file (e.g. ".vec.wpl")
     -> Doc        -- ^ Doc to dump
     -> m ()
dump dflags dflag ext doc | isDynFlagSet dflags dflag = liftIO $
    withDumpHandle $ \h -> hPutDoc h doc
  where
    withDumpHandle :: (Handle -> IO ()) -> IO ()
    withDumpHandle m
       | isDynFlagSet dflags StdoutDump = m stderr >> putStrLn ""
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
     , Option ['d']     ["debug"]               (NoArg Debug)         "debug"
     , Option []        ["debug-fold"]          (NoArg DebugFold)     "debug-fold"
     , Option ['x']     ["optimize"]            (NoArg Opt)           "optimize"
     , Option ['v']     ["verbose"]             (NoArg Verbose)       "verbose"
     , Option ['a']     ["bounds-check"]        (NoArg BoundsCheck)   "bounds check"
     , Option []        ["stdout-dump"]         (NoArg StdoutDump)    "dump to stdout instead of files"
     , Option []        ["ddump-vect"]          (NoArg DumpVect)      "dump vectorization output"
     , Option []        ["ddump-types"]         (NoArg DumpTypes)     "dump a typechecked version of program"
     , Option []        ["ddump-vect-types"]    (NoArg DumpVectTypes) "dump typechecked vectorized program"
     , Option []        ["ddump-ast"]           (NoArg DumpAst)       "dump the parsed AST"
     , Option []        ["ddump-ast-pretty"]    (NoArg DumpAstPretty) "dump the parsed AST (pretty-printed)"
     , Option []        ["vectorize"]           (NoArg Vectorize)     "vectorize program"
     , Option []        ["autolut"]             (NoArg AutoLUT)       "automatically convert function to use LUTs"
     , Option []        ["pipeline"]            (NoArg Pipeline)      "pipeline standalone computations"

     , Option []        ["native-mitigators"]   (NoArg NativeMitigators) "use native mitigators instead of source-based"
     , Option []        ["no-lut-hashing"]      (NoArg NoLUTHashing)     "do not hash lut generation"

     , Option []        ["dummy-thread"]        (NoArg DummyThread)   "generate dummy thread when pipelining"
     , Option []        ["ddump-to-file"]       (NoArg DumpToFile)    "dump to a file"
     , Option []        ["ddump-fold"]          (NoArg DumpFold)      "dump results of folding"
     , Option []        ["ddump-inline"]        (NoArg DumpInline)    "dump results of inlining"
     , Option []        ["ddump-pipeline"]      (NoArg DumpPipeline)  "dump results of pipelining"
     , Option []        ["ddump-lut"]           (NoArg DumpLUT)       "dump results of LUTting"
     , Option []        ["ddump-autolut"]       (NoArg DumpAutoLUT)   "dump results of auto-LUT"

     , Option []        ["no-exp-fold"]         (NoArg NoExpFold)     "do not fold/inline expressions"
     , Option []        ["no-fold"]             (NoArg NoFold)        "do not fold/inline computations and expressions"
     , Option []        ["no-lut"]              (NoArg NoLUT)         "do not construct LUTs"

     , Option []        ["mock-lut"]            (NoArg MockLUT)       "debugging help for LUTS (internal only)"

     , Option []        ["max-lut"]             (ReqArg parseMaxLUTOpt "SIZE")    "max lut size"
     , Option []        ["stack-threshold"]     (ReqArg parseStkThres  "SIZE")    "stack allocation threshold"
     , Option []        ["affinity-mask"]       (OptArg parseAffinityMask "MASK") "affinity mask"

     ]

usage :: String
usage = "Usage: wpl [OPTION...] files..."

parseAffinityMask :: Maybe String -> DynFlag
parseAffinityMask Nothing  = AffinityMask defaultAffinityMask
parseAffinityMask (Just i) = AffinityMask $ read i

parseStkThres :: String -> DynFlag
parseStkThres i = StackAllocThreshold (read i)

defaultMaxStkThreshold :: Int
defaultMaxStkThreshold = CgMonad.cMAX_STACK_ALLOC

defaultAffinityMask :: Int
defaultAffinityMask = 255

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
