-- -*- mode: haskell -*-

{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.Ziria.Parser.Lexer
-- Copyright   : (c) 2014-2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>
--
--------------------------------------------------------------------------------

module Language.Ziria.Parser.Lexer (
    lexToken,
    lexTokens
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Exception
import Control.Monad.State
import Data.Char (chr,
                  isDigit,
                  isHexDigit,
                  isOctDigit,
                  toLower)
import Data.List (intersperse)
import Data.Loc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Symbol
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Mainland

import Language.Ziria.Parser.Alex
import Language.Ziria.Parser.Monad
import Language.Ziria.Parser.Tokens
}

--
-- These definitions are lifted straight from the haskell Report
-- See https://www.haskell.org/definition/haskell98-report.pdf
--

$special   = [\(\)\,\;\[\]\`\{\}]

$uniWhite  = []
$whitechar = [\ \t\n\r\f\v]

$ascSmall = [a-z]
$uniSmall = []
$small    = [$ascSmall $uniSmall \_]

$ascLarge = [A-Z]
$uniLarge = []
$large    = [$ascLarge $uniLarge]

$alpha     = [$small $large]

$ascSymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$uniSymbol = []
$symbol    = [$ascSymbol $uniSymbol] # [$special \_\:\"\']

$ascDigit = [0-9]
$uniDigit = []
$digit    = [$ascDigit $uniDigit]
$octit    = [0-7]
$hexit    = [$digit A-F a-f]

$graphic  = [$small $large $symbol $digit $special \:\"\']

$idchar = [$small $large $digit \']
@id     = $alpha $idchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal | [eE] @decimal

-- For strings
$charesc  = [abfnrtv\\\"\'\&]
$cntrl    = [$ascLarge\@\[\\\]\^\_]
@ascii    = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
          | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
          | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
          | EM | SUB | ESC | FS | GS | RS | US | SP | DEL
@escape   = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)

-- For characters, since Alex doesn't accept the syntax @escape # [\\\&]
$ccharesc = [abfnrtv\"\']
@cescape  = \\ ($ccharesc | @ascii | @decimal | o @octal | x @hexadecimal)

@gap     = \\ $whitechar+ \\
@char    = \' ($graphic # [\'\\] | " " | @cescape) \'
@string  = \" ($graphic # [\"\\] | " " | @escape | @gap)* \"

ziria :-

<0> {
  ^ $whitechar* "#line" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .* { setLineFromPragma }
  ^ $whitechar* "#" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .*     { setLineFromPragma }

  $whitechar+ ;
  "--"\-*.*   ;

  "{-" { lexNestedComment }

  @id { identifier }

  @decimal            { lexConst TintConst }
  0[oO] @octal        { lexConst TintConst }
  0[xX] @hexadecimal  { lexConst TintConst }

  @decimal (\. @decimal @exponent? | @exponent) { lexConst TfloatConst }

  @char   { lexConst TcharConst }
  @string { lexConst TstringConst }

  "("   { token Tlparen }
  ")"   { token Trparen }
  "["   { token Tlbrack }
  "]"   { token Trbrack }
  "{"   { token Tlbrace }
  "}"   { token Trbrace }

  "!"   { token Tbang }
  "."   { token Tdot }
  ","   { token Tcomma }
  ";"   { token Tsemi }
  ":"   { token Tcolon }

  "+"   { token Tplus }
  "-"   { token Tminus }
  "*"   { token Tstar }
  "/"   { token Tdiv }
  "%"   { token Trem }
  "**"  { token Texp }
  "<<"  { token Tshiftl }
  ">>"  { token Tshiftr }

  "=="   { token Teq }
  "!="   { token Tne }
  "<"    { token Tlt }
  ">"    { token Tgt }
  "<="   { token Tle }
  ">="   { token Tge }

  "~"    { token Tbneg }
  "&"    { token Tband }
  "|"    { token Tbor }
  "^"    { token Tbxor }

  "&&"   { token Tland }
  "||"   { token Tlor }

  "="     { token Tdef }
  ":="    { token Tassign }
  "<-"    { token Tbind }
  ">>>"   { token Tcompose }
  "|>>>|" { token Tpcompose }

  "'0" { token TzeroBit }
  "'1" { token ToneBit }
}

{
keywords :: Map Symbol Token
keywords = Map.fromList kws
  where
    kws :: [(Symbol, Token)]
    kws = [ ("C",           TC)
          , ("ST",          TST)
          , ("T",           TT)
          , ("arr",         Tarr)
          , ("bit",         Tbit)
          , ("bool",        Tbool)
          , ("autoinline",  Tautoinline)
          , ("comp",        Tcomp)
          , ("complex",     Tcomplex)
          , ("complex8",    Tcomplex8)
          , ("complex16",   Tcomplex16)
          , ("complex32",   Tcomplex32)
          , ("complex64",   Tcomplex64)
          , ("do",          Tdo)
          , ("done",        Tdone)
          , ("double",      Tdouble)
          , ("else",        Telse)
          , ("emit",        Temit)
          , ("emits",       Temits)
          , ("error",       Terror)
          , ("external",    Texternal)
          , ("false",       Tfalse)
          , ("filter",      Tfilter)
          , ("for",         Tfor)
          , ("forceinline", Tforceinline)
          , ("fun",         Tfun)
          , ("if",          Tif)
          , ("in",          Tin)
          , ("int",         Tint)
          , ("int8",        Tint8)
          , ("int16",       Tint16)
          , ("int32",       Tint32)
          , ("int64",       Tint64)
          , ("length",      Tlength)
          , ("let",         Tlet)
          , ("map",         Tmap)
          , ("not",         Tnot)
          , ("noinline",    Tnoinline)
          , ("nounroll",    Tnounroll)
          , ("print",       Tprint)
          , ("println",     Tprintln)
          , ("read",        Tread)
          , ("repeat",      Trepeat)
          , ("return",      Treturn)
          , ("seq",         Tseq)
          , ("standalone",  Tstandalone)
          , ("struct",      Tstruct)
          , ("take",        Ttake)
          , ("takes",       Ttakes)
          , ("then",        Tthen)
          , ("times",       Ttimes)
          , ("true",        Ttrue)
          , ("unroll",      Tunroll)
          , ("until",       Tuntil)
          , ("begin",       Tbegin)
          , ("end",         Tend)
          , ("var",         Tvar)
          , ("while",       Twhile)
          , ("write",       Twrite)
          ]

identifier :: Action P Token
identifier beg end =
    case Map.lookup ident keywords of
      Nothing  -> token (Tidentifier ident) beg end
                  {- 
                  do x <- isStructId ident
                     if x then token (TstructIdentifier ident) beg end
                          else token (Tidentifier ident) beg end
                  -}

      Just tok -> token tok beg end
  where
    ident :: Symbol
    ident = intern (inputString beg end)

lexConst :: Read a => ((String, a) -> Token) -> Action P Token
lexConst tok beg end = do
    token (tok (s, read s)) beg end
  where
    s :: String
    s = inputString beg end

setLineFromPragma :: Action P Token
setLineFromPragma beg end = do
    inp <- getInput
    setInput inp { alexPos = pos' }
    lexToken
  where
    (_ : l : ws) = words (inputString beg end)
    line = read l - 1
    filename = (takeWhile (/= '\"') . drop 1 . concat . intersperse " ") ws

    pos' :: Pos
    pos' = Pos filename line 1 (posCoff (alexPos beg))

lexNestedComment :: Action P Token
lexNestedComment beg _ = do
    scan 1
    lexToken
  where
    scan :: Int -> P ()
    scan 0 =
        return ()

    scan n = do
        c <- nextChar
        case c of
          '-' -> do
              c2 <- nextChar
              case c2 of
                '}'  -> scan (n-1)
                _    -> scan n
          '{' -> do
              c2 <- nextChar
              case c2 of
                '-'  -> scan (n+1)
                _    -> scan n
          _ -> scan n

    retreatPos :: Pos -> Int -> Pos
    retreatPos (Pos f l c coff) n = Pos f l (c-n) (coff-n)

lexToken :: P (L Token)
lexToken = do
    beg  <- getInput
    st   <- get
    case alexScanUser st beg 0 of
      AlexEOF ->
          return $ L (Loc (alexPos beg) (alexPos beg)) Teof
      AlexError end ->
          lexerError end (text rest)
        where
          rest :: String
          rest = T.unpack $ T.take 80 $ T.drop (alexOff end) (alexBuf end)

      AlexSkip end _ ->
          setInput end >> lexToken

      AlexToken end _ t ->
          setInput end >> t beg end

lexTokens  ::  MonadException m
           =>  T.Text
           ->  Pos
           ->  m [L Token]
lexTokens buf start =
    liftException (evalP tokens (emptyPState buf start))
  where
    tokens :: P [L Token]
    tokens = do
        t <- lexToken
        case t of
          L _ Teof  -> return [t]
          _         -> (t :) <$> tokens
}
