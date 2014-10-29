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
-- | The lexer monad
--
-- Adapted from the "monad" wrapper bundled with Alex.
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module ZiriaLexerMonad (
    AlexPosn(..)
  , AlexState(..)
  , AlexInput
  , Alex
  , Lexeme(..)
  , LexemeClass(..)
  , alexError
  , alexGetByte
  , alexGetFileName
  , alexGetInput
  , alexGetStartCode
  , alexInputPrevChar
  , alexSetInput
  , alexSetFileName
  , alexSetStartCode
  , alexStartPos
  , ignorePendingBytes
  , lexError
  , mkAlexState
  , mkL
  , runAlex
  , runAlex'
  , showPosn
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Data.Char (ord, isSpace)
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import Data.Word (Word8)
import qualified Data.Bits

{-------------------------------------------------------------------------------
  Positions
-------------------------------------------------------------------------------}

-- | Token positions
--
-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file.
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

{-------------------------------------------------------------------------------
  Lexer input
-------------------------------------------------------------------------------}

type AlexInput = ( AlexPosn     -- current position,
                 , Char         -- previous char
                 , [Byte]       -- pending bytes on current char
                 , String       -- current input string
                 )

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, b:bs, s)   = Just (b,(p,c,bs,s))
alexGetByte (_, _, [],   [])  = Nothing
alexGetByte (p, _, [],   c:s) = let p'     = alexMove p c
                                    (b:bs) = utf8Encode c
                                in p' `seq` Just (b, (p', c, bs, s))

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

{-------------------------------------------------------------------------------
  Lexer state
-------------------------------------------------------------------------------}

data AlexState = AlexState {
      alex_pos   :: !AlexPosn     -- position at current input location
    , alex_inp   :: String        -- the current input
    , alex_chr   :: !Char         -- the character before the input
    , alex_bytes :: [Byte]
    , alex_scd   :: !Int          -- the current startcode
    , alex_fn    :: String        -- #cpp filename override (blank if none)
    }

alexStateGetInput :: AlexState -> AlexInput
alexStateGetInput AlexState{..} =
    (alex_pos, alex_chr, alex_bytes, alex_inp)

alexStateSetInput :: AlexInput -> AlexState -> AlexState
alexStateSetInput (alex_pos, alex_chr, alex_bytes, alex_inp) st =
    st { alex_pos   = alex_pos
       , alex_chr   = alex_chr
       , alex_bytes = alex_bytes
       , alex_inp   = alex_inp
       }

mkAlexState :: String -> AlexState
mkAlexState input = AlexState {
      alex_pos   = alexStartPos
    , alex_inp   = input
    , alex_chr   = '\n'
    , alex_bytes = []
    , alex_scd   = 0
    , alex_fn    = ""
    }

alexSetFileName :: String -> Alex ()
alexSetFileName alex_fn = modify $ \st -> st { alex_fn = alex_fn }

alexGetFileName :: Alex String
alexGetFileName = gets alex_fn

{-------------------------------------------------------------------------------
  The lexer monad proper
-------------------------------------------------------------------------------}

newtype Alex a = Alex { _unAlex :: ErrorT String (State AlexState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState AlexState
           , MonadError String
           )

runAlex :: Alex a -> AlexState -> (Either String a, AlexState)
runAlex (Alex act) st = runState (runErrorT act) st

runAlex' :: Alex Lexeme -> AlexState -> (Lexeme, AlexState)
runAlex' act st = first convErr $ runAlex act st
  where
    -- TODO: I don't know why alex is reporting the error starting at the
    -- whitespace before the token. For now we work around it here and report
    -- the error at the first non-whitespace character.
    convErr (Right a) = a
    convErr (Left  _) = let (sp, afterSp) = span isSpace (alex_inp st)
                            pos           = foldl' alexMove (alex_pos st) sp
                            err           = maybeToList $ listToMaybe afterSp
                        in L pos LError err

{-------------------------------------------------------------------------------
  Primitive operations in the Alex monad
-------------------------------------------------------------------------------}

alexGetInput :: Alex AlexInput
alexGetInput = gets alexStateGetInput

alexSetInput :: AlexInput -> Alex ()
alexSetInput = modify . alexStateSetInput

alexError :: String -> Alex a
alexError = throwError

alexGetStartCode :: Alex Int
alexGetStartCode = gets alex_scd

alexSetStartCode :: Int -> Alex ()
alexSetStartCode alex_scd = modify $ \st -> st { alex_scd = alex_scd }

lexError :: String -> Alex a
lexError s = do
  (p,_,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
                   (if (not (null input))
                     then " before " ++ show (head input)
                     else " at end of file"))

{-------------------------------------------------------------------------------
  Lexemes
-------------------------------------------------------------------------------}

data Lexeme = L AlexPosn         -- position
                LexemeClass      -- lexeme class
                String           -- the actual lexeme itself
            | StartOfFile        -- This is the only lexeme without a position

instance Show Lexeme where
  show StartOfFile = "start of file"
  show (L _ c s)   = go (\str -> str ++ " " ++ show s) c
    where
      go :: (String -> String) -> LexemeClass -> String
      go f LInteger    = f "integer"
      go f LFloat      = f "float"
      go f LChar       = f "character"
      go f LString     = f "string"
      go f LSpecial    = f "symbol"
      go f LReservedId = f "keyword"
      go f LReservedOp = f "operator"
      go f LVarId      = f "variable"
      go _ LEOF        = "end of file"
      go f LError      = f "lexical error at character"
      go f LFileChange = f "file change to"

data LexemeClass
  = LInteger
  | LFloat
  | LChar
  | LString
  | LSpecial
  | LReservedId
  | LReservedOp
  | LVarId
  | LEOF
  | LError
  | LFileChange
  deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return $ L p c (take len str)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

type Byte = Word8

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
