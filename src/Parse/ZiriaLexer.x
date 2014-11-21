{
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
-- | Lexical analyser
--
-- Adapted from the Haskell lexer example bundled with Alex.
{-# OPTIONS_GHC -fno-warn-amp #-}
module ZiriaLexer (scan) where
import Data.Char (chr)
import Control.Monad (liftM)
import Debug.Trace
import Utils
import ZiriaLexerMonad
}

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = 
  '0|'1|C|ST|T|arr|comp|do|else|emit|emits|error|external|false|filter|for|fun|
  if|in|length|let|map|not|nounroll|print|println|read|repeat|return|seq|
  standalone|struct|take|takes|then|times|true|unroll|until|var|while|write 

@reservedop =
  "!=" |  "%" |  "&" |  "&&" |  "*" |  "**" |  "+" |  "-" |  "." | 
  "/" |  ":" |  ":=" |  "<" |  "<<" |  "<=" |  "=" |  "==" |  ">" |  ">=" |
  ">>" |  "^" |  "|" |  "||" |  "~" | ">>>" | "|>>>|" | "!" | "<-"

@varid  = $small $idchar*
@conid  = $large $idchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+                     { skip }
<0> "--" .* \n                  { skip }

"{-"                            { nested_comment }

<0> $special                    { mkL LSpecial }
<0> @reservedid                 { mkL LReservedId }
<0> @varid                      { mkL LVarId }
<0> @conid                      { mkL LVarId } -- Ziria doesn't distinguish
<0> @reservedop                 { mkL LReservedOp }

<0> @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal          { mkL LInteger }

<0> @decimal \. @decimal @exponent?
  | @decimal @exponent          { mkL LFloat }

<0> \' ($graphic # [\'\\] | " " | @escape) \'
                                { mkL LChar }

<0> \" @string* \"              { mkL LString }

<0> "#" .* \n                   { processCppPragma }

{
{-------------------------------------------------------------------------------
  Top-level API for the lexer
-------------------------------------------------------------------------------}

scan :: AlexState -> (Lexeme, AlexState)
scan = runAlex' alexMonadScan

testScanner :: String -> IO ()
testScanner = go . mkAlexState 
  where
    go :: AlexState -> IO ()
    go st = do 
      let (l@(L _ c _), st') = scan st 
      case c of
        LEOF   -> print l
        LError -> print l
        _      -> do print l ; go st'

{-------------------------------------------------------------------------------
  Token actions
-------------------------------------------------------------------------------}

skip :: AlexInput -> Int -> Alex Lexeme
skip input len = alexMonadScan

processCppPragma :: AlexInput -> Int -> Alex Lexeme
processCppPragma (pos@(AlexPn addr _line col), prev, pending, str) len = do
    let (pragma, str') = splitAt len str 
    case parsePragmaLine pragma of
      Just (line', fn', _) -> do
        -- Update position
        let pos' = AlexPn (addr + len) line' col
        alexSetInput (pos', prev, pending, str') -- TODO: Change prev/pending?

        -- If filename changed, return special token to the parser
        -- (we allow for filename changes (i.e., #includes) only at top-level)
        fn <- alexGetFileName
        if fn == fn' 
          then alexMonadScan -- skip
          else do alexSetFileName fn' 
                  return $ L pos LFileChange fn' 
      Nothing -> do
        return $ L pos LError pragma 

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
    input <- alexGetInput
    go 1 input
  where 
    go 0 input = do alexSetInput input; alexMonadScan
    go n input = do
      case alexGetByte input of
        Nothing  -> err input
        Just (c,input) -> do
          case chr (fromIntegral c) of
            '-' -> do
              case alexGetByte input of
                Nothing  -> err input
                Just (125,input) -> go (n-1) input
                Just (c,input)   -> go n input
            '\123' -> do
              case alexGetByte input of
                Nothing  -> err input
                Just (c,input) | c == fromIntegral (ord '-') -> go (n+1) input
                Just (c,input)   -> go n input
            c -> go n input

    err input = do alexSetInput input; lexError "error in nested comment"  

{-------------------------------------------------------------------------------
  Monadic wrapper around the generated scanner
-------------------------------------------------------------------------------}

alexMonadScan :: Alex Lexeme
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexEOF :: Alex Lexeme
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return $ L pos LEOF ""
}
