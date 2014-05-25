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
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module SysTools
  ( compileAndRun
  , readProcessWithExitCode
  ) where

import Control.Concurrent (forkIO,
                           newEmptyMVar,
                           putMVar,
                           takeMVar)
import qualified Control.Exception as E
import Control.Monad (filterM, liftM2, when)
import Control.Monad.Trans (liftIO)
-- import System.Directory(getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.Process (StdStream(..),
                       proc,
                       createProcess,
                       waitForProcess,
                       std_in,
                       std_out,
                       std_err)
import System.IO (hClose,
                  hFlush,
                  hGetContents,
                  hPutStr, openFile, IOMode( .. ) 
                  )

import Data.Char ( isSpace )

import System.PosixCompat.Files (fileAccess, fileExist)
import Text.PrettyPrint.Mainland


-- Find the 'make' executable 
findMake :: IO FilePath
findMake = do
    executablePaths <- filterM isExecutable possiblePaths
    if (null executablePaths)
       then fail "Cannot find make!"
       else return (head executablePaths)
  where
    possiblePaths :: [FilePath]
    possiblePaths =
        ["C:\\cygwin\\bin\\make.exe"
        ,"/usr/bin/make"
        ,"/usr/local/bin/make"
        ,"make"
        ]
    isExecutable :: FilePath -> IO Bool
    isExecutable path =
        (fileExist path `andM` fileAccess path False False True)
        `E.catch` (\(e::E.SomeException) -> return False)
      where
        andM = liftM2 (&&)


-- Cat the C program into the top-level _csrc 
-- directory and call the appropriate makefile target.

compileAndRun :: String            -- ^ Program text
              -> String            -- ^ Makefile target from $(TOP)/_csrc


              -> FilePath          -- ^ Csrc path (posix-style for cygwin and linux)
              -> FilePath          -- ^ Csrc path (native-style, will be as the previous in Linux, 
                                   -- ^ but different in windows/cygwin)

              -> [(String,String)] -- ^ #defines
              -> IO String         -- ^ Standard output
compileAndRun source mk_tgt csrc_path_posix csrc_path_native defines = do

    h <- openFile (csrc_path_native ++ "/" ++ "test.c") WriteMode
    hPutStr h source
    hClose h
 
    let csrc_path_native' = map (\c -> if c == '\\' then '/' else c) csrc_path_native
    
    let args' = ["--directory=" ++ (dropWhile isSpace csrc_path_posix)] ++ [mk_tgt]
    putStrLn ("DEBUG:" ++ dropWhile isSpace csrc_path_posix);

    makePath <- findMake

    (ex, _, err) <- liftIO $ readProcessWithExitCode makePath args' ""

    when (ex /= ExitSuccess) $ do
       faildoc $ stack [ text "Cannot invoke `make' from wplc:" <+> (text . show) ex
                       , text "Error:" <+> text err ]

    (ex, out, err) 
      <- liftIO $ 
         readProcessWithExitCode (csrc_path_native ++ "/" ++ mk_tgt) [] ""

    when (ex /= ExitSuccess) $ do
       faildoc $  text "Cannot invoke temporary program:" <+>
                  (text . show) ex <+> string err

    return out

readProcessWithExitCode :: FilePath                     -- ^ command to run
                        -> [String]                     -- ^ any arguments
                        -> String                       -- ^ standard input
                        -> IO (ExitCode,String,String)  -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- forkIO $ E.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    _ <- forkIO $ E.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do
        hPutStr inh input
        hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)
