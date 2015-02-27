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

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error 
import GHC.IO.Exception

import Data.IORef 

import Control.Exception
import Text.Parsec
import Data.Maybe
import qualified Data.Map as M

import Foreign

import Control.Monad ( when, unless, foldM )

import Data.List ( nub )

data DynFlag =
    InFile String
  | GroundFile String
  | Verbose           
  | PrefixAllowed     -- PrefixAllowed means that # entries can differ
  | Debug             -- If set then Debug mode is on (else binary is assumed)
  | Help
  | NinetyPercent Double 
                      -- Compare files in debug mode, assuming int-valued
                      -- entries that they must be within each other by 
                      -- the percent expressed by the double [0.0 - 1.0]
  | Threshold Double 
                      -- When the absolute value is too small (close to zero)
                      -- the relative error will be arbitrary and we'll get false positives
                      -- To avoid this, set the threshold, and any mismatched values
                      -- that are below the threshold will be ignored
  | AsComplex
  deriving Eq

type DynFlags = [DynFlag]

isDynFlagSet :: DynFlags -> DynFlag -> Bool
isDynFlagSet flags f = f `elem` flags


options :: [OptDescr DynFlag]
options
  = [ Option ['f']     ["file"]       (ReqArg InFile "FILE")          "File"
    , Option ['g']     ["ground"]     (ReqArg GroundFile "FILE")      "Ground truth file"
    , Option ['d']     ["debug"]      (NoArg  Debug)                  "Text mode (see Blink IO spec); default off"
    , Option ['p']     ["prefix"]     (NoArg PrefixAllowed)           "Allows fewer entries in file than in ground; default off"
    , Option ['v']     ["verbose"]    (NoArg  Verbose)                "Verbose messages; default off"
    , Option ['h']     ["help"]       (NoArg Help)                    "Print help message"
    , Option ['n']     ["npc"]        (ReqArg parse_npc "precision")  "Precision 0.0-1.0, only valid when debug is on and entries are numerical"
    , Option ['t']     ["thr"]        (ReqArg parse_thr "threshold")  "Ignore numbers that are in absolute under threshold (i.e. too close to 0)"
    , Option ['c']     ["as-complex"] (NoArg AsComplex)               "When precision is on and debug is on, the precision test is done for complex numbers (i.e. pairs of entries at a time)"
    ]
  where parse_npc :: String -> DynFlag
        parse_npc s = NinetyPercent (read s)
        parse_thr :: String -> DynFlag
        parse_thr s = Threshold (read s)

usage = "Usage: blinkdiff [OPTION...] \n" ++ 
        "Exit codes: 0 = files match, 1 = mismatch, 2 = some error occured\n"

fullUsage = usageInfo usage options

failWithMsg msg
  = hPutStrLn stderr msg >> exitWith (ExitFailure 2)

mismatchWithMsg verbose_on msg
  = do { when verbose_on $
         putStrLn ("Mismatch: " ++ msg)
       ; exitWith (ExitFailure 1) }

getInFile :: DynFlags -> IO String
getInFile [] = failWithMsg $ "No input file given\n" ++ fullUsage
getInFile (InFile file : _) = return file
getInFile (_ : opts')       = getInFile opts'

getGndFile :: DynFlags -> IO String
getGndFile [] = failWithMsg $ "No ground file given\n" ++ fullUsage
getGndFile (GroundFile file : _) = return file
getGndFile (_ : opts')           = getGndFile opts'

getNpcFlag :: DynFlags -> IO (Maybe Double)
-- Returns Nothing if npc is not set, or 0-100 if npc is set
getNpcFlag []                    
  = return Nothing 
getNpcFlag (NinetyPercent n : _) 
  | 0.0 <= n, n <= 1.0
  = return (Just n)
  | otherwise 
  = failWithMsg $ "Ninety-percent must be in range 0.0-1.0\n" ++ fullUsage
getNpcFlag (_ : opts') 
  = getNpcFlag opts'


getThrFlag :: DynFlags -> IO (Maybe Double)
-- Returns Nothing if npc is not set, or 0-100 if npc is set
getThrFlag []                    
  = return Nothing 
getThrFlag (Threshold n : _) 
  | 0.0 <= n
  = return (Just n)
  | otherwise 
  = failWithMsg $ "Threshold must be positive\n" ++ fullUsage
getThrFlag (_ : opts') 
  = getThrFlag opts'


compilerOpts :: [String] -> IO (DynFlags, [String])
compilerOpts argv = 
  case getOpt Permute options argv of
         (o,n,[]  ) -> return (o,n)
         (_,_,errs) -> failWithMsg $ concat errs ++ fullUsage


-- Return codes:
-- 0 files match
-- 1 file mismatch
-- 2 some exception/error code
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main = do { hSetBuffering stdout NoBuffering
          ; catchIOError go (errHandler "Invalid parameters!") }
 where go = do { args <- getArgs
               ; (flags,_) <- compilerOpts args
               ; when (isDynFlagSet flags Help) $ failWithMsg fullUsage
               ; inFile  <- getInFile flags
               ; gndFile <- getGndFile flags
               ; npc <- getNpcFlag flags
               ; thr <- getThrFlag flags
               ; when ((isJust npc || isJust thr) && not (isDynFlagSet flags Debug)) $
                 failWithMsg $ 
                 "Ninety-percent and threshold can only be used in debug mode\n" ++ fullUsage
               ; doCompare flags npc thr inFile gndFile 
               }

-- Abnormal termination (exit code 2)
errHandler msg (e :: IOError)
  = do -- hPutStrLn stderr msg
       let fn = ioe_filename e
       when (isJust fn) $ hPutStr stderr (fromJust fn ++ ":")
       hPutStrLn stderr (ioe_description e) 
       exitWith (ExitFailure 2)

doCompare :: DynFlags -> Maybe Double -> Maybe Double -> String -> String -> IO ()
doCompare dflags npc thr infile gndfile 
  = withFiles infile gndfile $ 
    if debug_on then 
        if complex_on then
            do_compare prefix_on verbose_on $ textPairFmtReader npc thr
        else 
            do_compare prefix_on verbose_on $ textFmtReader npc thr
    else
        do_compare prefix_on verbose_on $ binFmtReader
  where
    debug_on   = isDynFlagSet dflags Debug
    complex_on = isDynFlagSet dflags AsComplex
    verbose_on = isDynFlagSet dflags Verbose
    prefix_on  = isDynFlagSet dflags PrefixAllowed
    open_file  = if debug_on then openFile else openBinaryFile
 
    withFiles ifile gfile act = 
      catchIOError (do { hin  <- open_file ifile ReadMode
                       ; hgnd <- open_file gfile ReadMode
                       ; act hin hgnd 
                       ; hClose hin >> hClose hgnd }) $
      errHandler "Cannot open file!"



-- Main comparison loop 
do_compare :: Show b
           => Bool  -- Prefix  flag 
           -> Bool  -- Verbose flag
           -> FmtReader s b -> Handle -> Handle -> IO ()
-- Post: Should never raise an IOError
do_compare prefix_on verbose_on fmtr hin hgnd 
  = do s1 <- init_state fmtr -- Initialize two readers
       s2 <- init_state fmtr 
       aref <- newIORef (1.0 :: Double) 
       go aref 0 (s1,s2)
  where 
    go aref cnt (s1,s2)
     = do { hin_eof  <- hIsEOF hin
          ; hgnd_eof <- hIsEOF hgnd
          ; if hin_eof then
               if prefix_on then 
                   do { let msg = if hgnd_eof then "Matching! (EOF)"
                                  else "Matching! (Prefix of " ++ 
                                            show cnt ++ " entries)"
                      ; when verbose_on $ 
                        do { putStr msg
                           ; r <- readIORef aref 
                           ; putStrLn (" (Accuracy " ++ show (to_pc r) ++"%)") }
                      ; exitWith ExitSuccess }
               else if hgnd_eof then 
                      do { when verbose_on $ 
                           do { putStr "Matching! (EOF)"
                              ; r <- readIORef aref 
                              ; putStrLn 
                                   (" (Accuracy "++show (to_pc r) ++"%)") }
                         ; exitWith ExitSuccess }
                    else mismatchWithMsg verbose_on $
                         "file shorter than ground!"
            else 
                do { mb_ival <- read_entry fmtr s1 hin
                   ; when hgnd_eof (mismatchWithMsg verbose_on "ground shorter than file!")
                   ; mb_gval <- read_entry fmtr s2 hgnd
                   ; case (mb_ival,mb_gval) of 
                        (Nothing,_) -> 
                           failWithMsg "Parse error! Bad input file" 
                        (_,Nothing) ->
                           failWithMsg "Parse error! Bad ground file" 
                        (Just ival,Just gval) -> 
                           case eq_entry fmtr ival gval of 
                             CmpEq -> go aref (cnt+1) (s1,s2)
                             CmpWithin r -> 
                               do { rstored <- readIORef aref
                                  ; when (r < rstored) $ writeIORef aref r 
                                  ; go aref (cnt+1) (s1,s2) }
                             CmpDiff -> 
                               mismatchWithMsg verbose_on $
                               ("at entry " ++ show cnt ++ "\n" ++ 
                                   "(file) " ++ show ival ++ " - (ground) " ++ show gval)
                   } }
    -- Convert a double from 0.0-1.0 to 00.00-100.00%
    to_pc (r :: Double) = 
      let round_up :: Integer = round (10000.0 * r) 
      in fromIntegral round_up / 100.00

{- Format readers
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  A Format Reader consists of an initialization function, a reader for
  the next entry and a comparison function that can compare two entries. 
-} 
data FmtReader s a = 
  MkFmtReader { 
      init_state  :: IO s
      -- Pre: handle is not EOF, Post: never raises an IOError 
    , read_entry  :: s -> Handle -> IO (Maybe a) 
    , eq_entry    :: a -> a -> CmpRes }

-- Comparison result
data CmpRes = 
   CmpEq             -- Exactly the same
 | CmpWithin Double  -- Entries are close by a percent
                     -- The threshold is something that 
                     -- each FmtReader knows about 
 | CmpDiff           -- Definitely different


-- A Binary Format Reader
-- ~~~~~~~~~~~~~~~~~~~~~~
binFmtReader :: FmtReader (ForeignPtr Word8) Word8
binFmtReader 
  = MkFmtReader { init_state  = mallocForeignPtrBytes 1 
                , read_entry  = bin_read_entry 
                , eq_entry    = \x y -> if x == y then CmpEq else CmpDiff }

bin_read_entry :: ForeignPtr Word8 -> Handle -> IO (Maybe Word8)
bin_read_entry fptr h 
  = catchIOError do_read $ \(e :: IOError) -> return Nothing
  where 
     do_read 
        = do { mlen <- withForeignPtr fptr hget
             ; if mlen == 1 then
                   do { val <- withForeignPtr fptr hpeek
                      ; return $ Just val }
               else return Nothing }
     hget buf  = hGetBuf h buf 1
     hpeek buf = peekElemOff buf 0 

-- A Text Format Reader
-- ~~~~~~~~~~~~~~~~~~~~
-- We pass on the NPC and THR flags because the comparison function may need to
-- compare for similarity up to a certain percentage (given by NPC)
-- instead of absolute equality, and only for entries larger than THR in absolute.
-- A typical example where we need THR is when we expect a zero but, 
-- due to a numerical error, get a small number (e.g. 1). This has infinite relative
-- error but no effect on the correctness. We ignore these corner cases through THR


textFmtReader :: Maybe Double -> Maybe Double -> FmtReader () String
textFmtReader npc thr
  = MkFmtReader { init_state = return ()
                , read_entry = txt_read_entry
                , eq_entry   = txt_cmp_entry npc thr}
txt_read_entry _unit h = go []
  where 
    go acc = 
      do b <- hIsEOF h
         if b then return (Just $ reverse acc)
         else do { x <- hGetChar h 
                 ; if x == '\\' then 
                       go_special acc
                   else if x == ',' 
                        then return (Just $ reverse acc)
                        else if is_whitespace x 
                             then go acc else go (x:acc) }
     -- Read one more character to get a special character
    go_special acc 
       = do { b <- hIsEOF h
            ; if b then illegal_escape_fail
              else do { x' <- hGetChar h
                      ; case compute_special_char x' of
                          Just c -> go (c:acc)
                          Nothing -> illegal_escape_fail
                      }}
    illegal_escape_fail 
       = failWithMsg "Illegal escape sequence!"

    is_whitespace x = (x == '\n' || x == ' ' || x == '\t' || x == '\r')
    compute_special_char 'n'  = Just '\n'
    compute_special_char ' '  = Just ' '
    compute_special_char 't'  = Just '\t'
    compute_special_char '\\' = Just '\\'
    compute_special_char 'r'  = Just '\r'
    compute_special_char ','  = Just ','
    compute_special_char _    = Nothing

txt_cmp_entry Nothing _ x y = if x == y then CmpEq else CmpDiff
txt_cmp_entry (Just pc) Nothing x y = txt_cmp_entry (Just pc) (Just 0) x y 
txt_cmp_entry (Just pc) (Just thr) x y 
 = case (reads x, reads y) of
     ([(dx::Double,[])],[(dy::Double,[])]) -> 
          let delta = abs (dx - dy)
          in if (abs(dx) < thr && abs(dy) < thr)
             then CmpWithin 1.0 else
               if delta <= abs(dy) * (1.0 - pc)
               then CmpWithin (1.0 - (delta /dy)) else CmpDiff
     -- We could just fail here and complain that we could not parse the file
     (_,_) -> error $ "Unparseable files: " ++ x ++ " - " ++ y 


-- Pair Format Reader
textPairFmtReader :: Maybe Double -> Maybe Double -> FmtReader () (Integer,Integer)
textPairFmtReader npc thr
  = MkFmtReader { init_state = return ()
                , read_entry = txtpair_read_entry
                , eq_entry   = txtpair_cmp_entry npc thr}

-- Read two entries at a time
txtpair_read_entry _unit h 
  = do mh1 <- txt_read_entry _unit h
       case mh1 of 
         Nothing -> return Nothing 
         Just v1 -> do mh2 <- txt_read_entry _unit h
                       case mh2 of 
                         Nothing -> return Nothing
                         Just v2 -> return $ Just (read v1, read v2)

txtpair_cmp_entry Nothing _ x y = if x == y then CmpEq else CmpDiff
txtpair_cmp_entry (Just pc) Nothing x y = txtpair_cmp_entry (Just pc) (Just 0) x y
txtpair_cmp_entry (Just pc) (Just thr) (x1,x2) (y1,y2)
 = let x1d :: Double = fromIntegral x1
       x2d :: Double = fromIntegral x2
       y1d :: Double = fromIntegral y1
       y2d :: Double = fromIntegral y2
       absx = sqrt (x1d^2 + x2d^2)
       absy = sqrt (y1d^2 + y2d^2)
       delta = sqrt ((x1d - y1d)^2 + (x2d - y2d)^2)
       ymes  = sqrt (y1d^2 + y2d^2)
   in if (absx < thr && absy < thr)
      then CmpWithin 1.0 else
        if delta <= ymes * (1.0 - pc) 
        then CmpWithin (1.0 - (delta /ymes)) else CmpDiff

