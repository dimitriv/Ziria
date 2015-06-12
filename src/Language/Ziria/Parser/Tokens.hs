-- |
-- Module      : Language.Ziria.Parser.Tokens
-- Copyright   : (c) 2014-2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>

module Language.Ziria.Parser.Tokens (
    Token(..)
  ) where

import Data.Symbol
import Text.PrettyPrint.Mainland

data Token = Teof
           | TintConst (String, Integer)
           | TfloatConst (String, Double)
           | TcharConst (String, Char)
           | TstringConst (String, String)
           | Tidentifier Symbol
           | TcompIdentifier Symbol

           | TzeroBit
           | ToneBit
           | TC
           | TST
           | TT
           | Tarr
           | Tautoinline
           | Tbit
           | Tbool
           | Tcomp
           | Tcomplex
           | Tcomplex8
           | Tcomplex16
           | Tcomplex32
           | Tcomplex64
           | Tdo
           | Tdone
           | Tdouble
           | Telse
           | Temit
           | Temits
           | Terror
           | Texternal
           | Tfalse
           | Tfilter
           | Tfor
           | Tforceinline
           | Tfun
           | Tif
           | Tin
           | Tint
           | Tint8
           | Tint16
           | Tint32
           | Tint64
           | Tlength
           | Tlet
           | Tmap
           | Tnot
           | Tnoinline
           | Tnounroll
           | Tprint
           | Tprintln
           | Tread
           | Trepeat
           | Treturn
           | Tseq
           | Tstandalone
           | Tstruct
           | Ttake
           | Ttakes
           | Tthen
           | Ttimes
           | Ttrue
           | Tunroll
           | Tuntil
           | Tvar
           | Twhile
           | Twrite

           | Tbegin
           | Tend

           | Tplus   -- ^ Addition
           | Tminus  -- ^ Subtraction
           | Tstar   -- ^ Multiplication
           | Tdiv    -- ^ Division
           | Trem    -- ^ Remainder
           | Texp    -- ^ Exponentiation
           | Tshiftl -- ^ Shift right
           | Tshiftr -- ^ Shift left

           | Teq -- ^ equal
           | Tne -- ^ Not equal
           | Tlt -- ^ Less than
           | Tgt -- ^ Greater than
           | Tle -- ^ Less than or equal
           | Tge -- ^ Greater than or equal

           | Tbneg -- ^ Bit-wise negation
           | Tband -- ^ Bit-wise and
           | Tbor  -- ^ Bit-wise or
           | Tbxor -- ^ Bit-wise xor

           | Tland -- ^ Logical and
           | Tlor  -- ^ Logical or

           | Tdef      -- ^ Bind (=)
           | Tassign   -- ^ Assignment (:=)
           | Tbind     -- ^ Bind (<-)
           | Tcompose  -- ^ Composition (>>>)
           | Tpcompose -- ^ Parallel composition (|>>>|)

           | Tlparen
           | Trparen
           | Tlbrack
           | Trbrack
           | Tlbrace
           | Trbrace

           | Tbang
           | Tdot
           | Tcomma
           | Tsemi
           | Tcolon
  deriving (Eq, Ord, Read, Show)

instance Pretty Token where
    ppr Teof = text "end of file"

    ppr (TintConst (s, _))    = text s
    ppr (TfloatConst (s, _))  = text s
    ppr (TcharConst (s, _))   = text s
    ppr (TstringConst (s, _)) = text s
    ppr (Tidentifier sym)     = (text . unintern) sym
    ppr (TcompIdentifier sym) = (text . unintern) sym

    ppr TzeroBit     = text "'0"
    ppr ToneBit      = text "'1"
    ppr TC           = text "C"
    ppr TST          = text "ST"
    ppr TT           = text "T"
    ppr Tarr         = text "arr"
    ppr Tautoinline  = text "autoinline"
    ppr Tbit         = text "bit"
    ppr Tbool        = text "bool"
    ppr Tcomp        = text "comp"
    ppr Tcomplex     = text "complex"
    ppr Tcomplex8    = text "complex8"
    ppr Tcomplex16   = text "complex16"
    ppr Tcomplex32   = text "complex32"
    ppr Tcomplex64   = text "complex64"
    ppr Tdo          = text "do"
    ppr Tdone        = text "done"
    ppr Tdouble      = text "double"
    ppr Telse        = text "else"
    ppr Temit        = text "emit"
    ppr Temits       = text "emits"
    ppr Terror       = text "error"
    ppr Texternal    = text "external"
    ppr Tfalse       = text "false"
    ppr Tfilter      = text "filter"
    ppr Tfor         = text "for"
    ppr Tforceinline = text "Tforceinline"
    ppr Tfun         = text "fun"
    ppr Tif          = text "if"
    ppr Tin          = text "in"
    ppr Tint         = text "int"
    ppr Tint8        = text "int8"
    ppr Tint16       = text "int16"
    ppr Tint32       = text "int32"
    ppr Tint64       = text "int64"
    ppr Tlength      = text "length"
    ppr Tlet         = text "let"
    ppr Tmap         = text "map"
    ppr Tnot         = text "not"
    ppr Tnoinline    = text "noinline"
    ppr Tnounroll    = text "nounroll"
    ppr Tprint       = text "print"
    ppr Tprintln     = text "println"
    ppr Tread        = text "read"
    ppr Trepeat      = text "repeat"
    ppr Treturn      = text "return"
    ppr Tseq         = text "seq"
    ppr Tstandalone  = text "standalone"
    ppr Tstruct      = text "struct"
    ppr Ttake        = text "take"
    ppr Ttakes       = text "takes"
    ppr Tthen        = text "then"
    ppr Ttimes       = text "times"
    ppr Ttrue        = text "true"
    ppr Tunroll      = text "unroll"
    ppr Tuntil       = text "until"
    ppr Tvar         = text "var"
    ppr Twhile       = text "while"
    ppr Twrite       = text "write"

    ppr Tplus   = text "+"
    ppr Tminus  = text "-"
    ppr Tstar   = text "*"
    ppr Tdiv    = text "/"
    ppr Trem    = text "%"
    ppr Texp    = text "**"
    ppr Tshiftl = text "<<"
    ppr Tshiftr = text ">>"

    ppr Tbegin  = text "begin"
    ppr Tend    = text "end"

    ppr Teq = text "=="
    ppr Tne = text "!="
    ppr Tlt = text "<"
    ppr Tgt = text ">"
    ppr Tle = text "<="
    ppr Tge = text ">="

    ppr Tbneg = text "~"
    ppr Tband = text "&"
    ppr Tbor  = text "|"
    ppr Tbxor = text "^"

    ppr Tland = text "&&"
    ppr Tlor  = text "||"

    ppr Tdef      = text "="
    ppr Tassign   = text ":="
    ppr Tbind     = text "<-"
    ppr Tcompose  = text ">>>"
    ppr Tpcompose = text "|>>>|"

    ppr Tlparen = text "("
    ppr Trparen = text ")"
    ppr Tlbrack = text "["
    ppr Trbrack = text "]"
    ppr Tlbrace = text "{"
    ppr Trbrace = text "}"

    ppr Tbang  = text "!"
    ppr Tdot   = text "."
    ppr Tcomma = text ","
    ppr Tsemi  = text ";"
    ppr Tcolon = text ":"
