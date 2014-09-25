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
-- As an exception we don't check for missing signatures here because most
-- definitions are simply (near) synonyms for the parsec equivalents.
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
-- | Lexical analysis
module BlinkLexer (
    angles
  , braces
  , brackets
  , charLiteral
  , colon
  , comma
  , commaSep
  , commaSep1
  , decimal
  , dot
  , float
  , hexadecimal
  , identifier
  , integer
  , lexeme
  , natural
  , naturalOrFloat
  , octal
  , operator
  , parens
  , reserved
  , reservedOp
  , semi
  , semiSep
  , semiSep1
  , stringLiteral
  , symbol
  , whiteSpace
  ) where

import Text.Parsec

import BlinkParseM
import qualified BlinkToken as P

{-------------------------------------------------------------------------------
  Language definitions
-------------------------------------------------------------------------------}

blinkStyle :: P.GenLanguageDef String BlinkParseState BlinkParseM
blinkStyle =
    emptyDef { P.commentStart     = "{-"
             , P.commentEnd       = "-}"
             , P.commentLine      = "--"
             , P.nestedComments   = True
             , P.identStart       = letter
             , P.identLetter      = alphaNum <|> oneOf "_'"
             , P.opStart          = oneOf $ map head allops
             , P.opLetter         = oneOf $ concatMap tail allops
             , P.reservedNames    = blinkReservedNames
             , P.reservedOpNames  = allops } -- Why not put all of them in?
  where
    allops      = unops ++ binops ++ reservedops
    reservedops = [":=",":","=","=>","<-"]

    emptyDef   :: P.LanguageDef st
    emptyDef    = P.LanguageDef
                   { P.commentStart   = ""
                   , P.commentEnd     = ""
                   , P.commentLine    = ""
                   , P.nestedComments = True
                   , P.identStart     = letter <|> char '_'
                   , P.identLetter    = alphaNum <|> oneOf "_'"
                   , P.opStart        = P.opLetter emptyDef
                   , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                   , P.reservedOpNames= []
                   , P.reservedNames  = []
                   , P.caseSensitive  = True
                   }

    unops  = [ "-", "~" ]
    binops = [ "**", "*", "/", "%", "+", "-", "<<", ">>", "<"
             , ">", ">=", "<=", "&", "^", "|", "==", "!=", "&&", "||"
             , ">>>", "|>>>|"
             ]

    blinkReservedNames =
      [ -- Computation language keywords
        "let", "comp", "in", "if", "then", "else", "read", "write"
      , "emit", "emits", "take", "takes", "while", "times", "repeat"
      , "until", "seq", "do", "external", "map", "filter", "fun"

        -- Expression language keywords
      , "return", "length", "bperm", "for", "lut", "print", "unroll", "nounroll"
      , "println", "error", "true", "false", "'0", "'1", "while"

        -- Types
      , "arr", "struct"
      , "ST", "C", "T"
      ]

{-------------------------------------------------------------------------------
  Parsec bindings
-------------------------------------------------------------------------------}

blinkLexer :: P.GenTokenParser String BlinkParseState BlinkParseM
blinkLexer = P.makeTokenParser blinkStyle

angles         = P.angles         blinkLexer
braces         = P.braces         blinkLexer
brackets       = P.brackets       blinkLexer
charLiteral    = P.charLiteral    blinkLexer
colon          = P.colon          blinkLexer
comma          = P.comma          blinkLexer
commaSep       = P.commaSep       blinkLexer
commaSep1      = P.commaSep1      blinkLexer
decimal        = P.decimal        blinkLexer
dot            = P.dot            blinkLexer
float          = P.float          blinkLexer
hexadecimal    = P.hexadecimal    blinkLexer
identifier     = P.identifier     blinkLexer
integer        = P.integer        blinkLexer
lexeme         = P.lexeme         blinkLexer
natural        = P.natural        blinkLexer
naturalOrFloat = P.naturalOrFloat blinkLexer
octal          = P.octal          blinkLexer
operator       = P.operator       blinkLexer
parens         = P.parens         blinkLexer
reserved       = P.reserved       blinkLexer
reservedOp     = P.reservedOp     blinkLexer
semi           = P.semi           blinkLexer
semiSep        = P.semiSep        blinkLexer
semiSep1       = P.semiSep1       blinkLexer
stringLiteral  = P.stringLiteral  blinkLexer
symbol         = P.symbol         blinkLexer
whiteSpace     = P.whiteSpace     blinkLexer
