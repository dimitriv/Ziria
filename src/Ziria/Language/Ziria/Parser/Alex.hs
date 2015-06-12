-- |
-- Module      : Language.Ziria.Parser.Alex
-- Copyright   : (c) 2014-2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ziria.Language.Ziria.Parser.Alex (
    AlexInput,
    alexInput,
    alexBuf,
    alexPos,
    alexOff,

    alexGetChar,
    alexGetByte,
    alexInputPrevChar,

    Action,
    locateTok,
    token,
    token1,
    inputString,
    inputText,

    Radix,
    decDigit,
    octDigit,
    hexDigit,
    decimal,
    octal,
    hexadecimal,

    readInteger,
    readDecimal,
    readRational
  ) where

import Data.Bits
import Data.Char (isDigit,
                  isOctDigit,
                  isHexDigit,
                  ord)
import Data.Int (Int64)
import Data.Loc (L(..), Loc(..), Pos(..), advancePos)
import Data.Ratio ((%))
import qualified Data.Text.Lazy as T
import Data.Word (Word8)

data AlexInput = AlexInput
  { alexBuf       :: !T.Text
  , alexPos       :: {-#UNPACK#-} !Pos
  , alexOff       :: {-#UNPACK#-} !Int64
  , alexPrevChar  :: {-#UNPACK#-} !Char
  , alexBytes     :: ![Word8]
  }

alexInput :: T.Text -> Pos -> AlexInput
alexInput bs pos = AlexInput
  { alexBuf       = bs
  , alexPos       = pos
  , alexOff       = 0
  , alexPrevChar  = '\n'
  , alexBytes     = []
  }

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar inp =
    case T.uncons (alexBuf inp) of
      Nothing      -> Nothing
      Just (c, bs) -> Just (c, inp { alexBuf       = bs
                                   , alexPos       = advancePos (alexPos inp) c
                                   , alexOff       = alexOff inp + 1
                                   , alexPrevChar  = c
                                   })

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@(AlexInput { alexBytes = [] }) =
    case alexGetChar inp of
      Nothing        -> Nothing
      Just (c, inp') -> Just (b, inp' { alexBytes = bs })
                          where
                            b : bs = utf8Encode c
  where
    utf8Encode :: Char -> [Word8]
    utf8Encode = map fromIntegral . go . ord
     where
       go :: Int -> [Int]
       go oc
           | oc <= 0x7f   = [oc]

           | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
                            , 0x80 + oc .&. 0x3f
                            ]

           | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]
           | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                            , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]

alexGetByte inp@(AlexInput { alexBytes = b:bs }) =
    Just (b, inp { alexBytes = bs })

alexInputPrevChar :: AlexInput -> Char
{-# INLINE alexInputPrevChar #-}
alexInputPrevChar = alexPrevChar

type Action m t = AlexInput -> AlexInput -> m (L t)

locateTok :: AlexInput -> AlexInput -> t -> L t
{-# INLINE locateTok #-}
locateTok beg end tok =
    L (Loc (alexPos beg) (alexPos end)) tok

token :: Monad m => t -> Action m t
{-# INLINE token #-}
token tok beg end =
    return $ locateTok beg end tok

token1 :: Monad m => (T.Text -> t) -> Action m t
{-# INLINE token1 #-}
token1 tok beg end =
    return $ locateTok beg end (tok (inputText beg end))

inputString :: AlexInput -> AlexInput -> String
{-# INLINE inputString #-}
inputString beg end = T.unpack $ inputText beg end

inputText :: AlexInput -> AlexInput -> T.Text
{-# INLINE inputText #-}
inputText beg end = T.take (alexOff end - alexOff beg) (alexBuf beg)

type Radix = (Integer, Char -> Bool, Char -> Int)

decDigit :: Char -> Int
decDigit c  | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c  | c >= '0' && c <= '7' = ord c - ord '0'
            | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c  | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
            | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
            | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in hexadecimal constant"

decimal :: Radix
decimal = (10, isDigit, decDigit)

octal :: Radix
octal = (8, isOctDigit, octDigit)

hexadecimal :: Radix
hexadecimal = (16, isHexDigit, hexDigit)

readSigned :: Num a => ReadS a -> ReadS a
readSigned rd ('+' : s) = rd s
readSigned rd ('-' : s) = do  (x, t) <- rd s
                              return (-x, t)
readSigned rd s         = rd s

readInteger :: Radix -> ReadS Integer
readInteger (radix, isRadixDigit, charToInt) =
    readSigned (go 0)
  where
    go :: Integer -> ReadS Integer
    go  x  []             = return (x, "")
    go  x  (c : cs)
        | isRadixDigit c  = go (x * radix + toInteger (charToInt c)) cs
        | otherwise       = return (x, c : cs)

readDecimal :: ReadS Integer
readDecimal = readInteger decimal

readRational :: ReadS Rational
readRational = readSigned readRat
  where
    readRat :: ReadS Rational
    readRat s = do
        (n, d, t)  <- readFix s
        (x, _)     <- readExponent t
        return ((n % 1) * 10^^(x - toInteger d), t)

    readFix :: String ->  [(Integer, Int, String)]
    readFix s =
        return (read (i ++ f), length f, u)
      where
        (i, t) = span isDigit s
        (f, u) = case t of
                   '.' : u' -> span isDigit u'
                   _        -> ("", t)

    readExponent :: ReadS Integer
    readExponent ""                        = return (0, "")
    readExponent (e : s0) | e `elem` "eE"  = readSigned readDecimal s0
                          | otherwise      = return (0, s0)
