-- -*- mode: haskell -*-

{
{-# OPTIONS -w #-}

-- |
-- Module      : Language.Ziria.Parser.Parser
-- Copyright   : (c) 2014-2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>

module Language.Ziria.Parser.Parser (
    parseExp,
    parseStmtBlock,
    parseProgram
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_,
                      when,
                      unless,
                      liftM)
import Control.Monad.Exception
import Data.List (foldl1',
                  intersperse)
import Data.Loc
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid
import Data.Symbol
import Text.PrettyPrint.Mainland

import Language.Ziria.Parser.Lexer
import Language.Ziria.Parser.Monad
import qualified Language.Ziria.Parser.Tokens as T

import AstExpr
import AstComp
import AstUnlabelled
import Interpreter (evalSrcInt)
}

%token
  CHAR        { L _ (T.TcharConst _) }
  STRING      { L _ (T.TstringConst _) }
  INT         { L _ (T.TintConst _) }
  FLOAT       { L _ (T.TfloatConst _) }
  ID          { L _ (T.Tidentifier _) }
  COMPID      { L _ (T.TcompIdentifier _) }

  "'0"          { L _ T.TzeroBit }
  "'1"          { L _ T.ToneBit }
  'C'           { L _ T.TC }
  'ST'          { L _ T.TST }
  'T'           { L _ T.TT }
  'arr'         { L _ T.Tarr }
  'autoinline'  { L _ T.Tautoinline }
  'bit'         { L _ T.Tbit }
  'bool'        { L _ T.Tbool }
  'comp'        { L _ T.Tcomp }
  'complex'     { L _ T.Tcomplex }
  'complex8'    { L _ T.Tcomplex8 }
  'complex16'   { L _ T.Tcomplex16 }
  'complex32'   { L _ T.Tcomplex32 }
  'complex64'   { L _ T.Tcomplex64 }
  'do'          { L _ T.Tdo }
  'double'      { L _ T.Tdouble }
  'else'        { L _ T.Telse }
  'emit'        { L _ T.Temit }
  'emits'       { L _ T.Temits }
  'error'       { L _ T.Terror }
  'external'    { L _ T.Texternal }
  'false'       { L _ T.Tfalse }
  'filter'      { L _ T.Tfilter }
  'for'         { L _ T.Tfor }
  'forceinline' { L _ T.Tforceinline }
  'fun'         { L _ T.Tfun }
  'if'          { L _ T.Tif }
  'in'          { L _ T.Tin }
  'int'         { L _ T.Tint }
  'int8'        { L _ T.Tint8 }
  'int16'       { L _ T.Tint16 }
  'int32'       { L _ T.Tint32 }
  'int64'       { L _ T.Tint64 }
  'length'      { L _ T.Tlength }
  'let'         { L _ T.Tlet }
  'map'         { L _ T.Tmap }
  'not'         { L _ T.Tnot }
  'noinline'    { L _ T.Tnoinline }
  'nounroll'    { L _ T.Tnounroll }
  'print'       { L _ T.Tprint }
  'println'     { L _ T.Tprintln }
  'read'        { L _ T.Tread }
  'repeat'      { L _ T.Trepeat }
  'return'      { L _ T.Treturn }
  'seq'         { L _ T.Tseq }
  'standalone'  { L _ T.Tstandalone }
  'struct'      { L _ T.Tstruct }
  'take'        { L _ T.Ttake }
  'takes'       { L _ T.Ttakes }
  'then'        { L _ T.Tthen }
  'times'       { L _ T.Ttimes }
  'true'        { L _ T.Ttrue }
  'unroll'      { L _ T.Tunroll }
  'until'       { L _ T.Tuntil }
  'var'         { L _ T.Tvar }
  'while'       { L _ T.Twhile }
  'write'       { L _ T.Twrite }

  '+'  { L _ T.Tplus }
  '-'  { L _ T.Tminus }
  '*'  { L _ T.Tstar }
  '/'  { L _ T.Tdiv }
  '%'  { L _ T.Trem }
  '**' { L _ T.Texp }
  '<<' { L _ T.Tshiftl }
  '>>' { L _ T.Tshiftr }

  '=='  { L _ T.Teq }
  '!='  { L _ T.Tne }
  '<'   { L _ T.Tlt }
  '>'   { L _ T.Tgt }
  '<='  { L _ T.Tle }
  '>='  { L _ T.Tge }

  '~' { L _ T.Tbneg }
  '&' { L _ T.Tband }
  '|' { L _ T.Tbor }
  '^' { L _ T.Tbxor }

  '&&' { L _ T.Tland }
  '||' { L _ T.Tlor }

  '='     { L _ T.Tdef }
  ':='    { L _ T.Tassign }
  '<-'    { L _ T.Tbind }
  '>>>'   { L _ T.Tcompose }
  '|>>>|' { L _ T.Tpcompose }

  '('  { L _ T.Tlparen }
  ')'  { L _ T.Trparen }
  '['  { L _ T.Tlbrack }
  ']'  { L _ T.Trbrack }
  '{'  { L _ T.Tlbrace }
  '}'  { L _ T.Trbrace }

  '!'  { L _ T.Tbang }
  '.'  { L _ T.Tdot }
  ','  { L _ T.Tcomma }
  ';'  { L _ T.Tsemi }
  ':'  { L _ T.Tcolon }

%right IF

%left ','
%left '&&' '||'
%left '==' '!='
%left '|'
%left '^'
%left '&'
%left '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%' '**'
%left LENGTH
%left NEG

%left '|>>>|'
%left '>>>'
%left STANDALONE

-- We expect shift/reduce conflicts for:
-- let expressions
-- if then else expressions
--%expect 2

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseExp       exp
%name parseStmtBlock stmt_block
%name parseProgram   program

%%
-- Ambiguities in the grammar
--
-- This grammar is somewhat painfully ambiguous. We use a variant of the C lexer
-- hack to get around the issue, but that just papers over the problem---it just
-- happens to correctly parse everything in the test suite. Some issues:
--
-- 1) The old grammar allowed expressions and statement block to occur in the
-- same context, and expressions could include array values. Unfortunately,
-- array values look a lot like statement blocks, because they can both start
-- with "{ f(x)...", for example. The exp_na nonterminal in the below grammar
-- skirts this issue by disallowing value constants in many circumstances.
--
-- 2) The main issue is that functions and computations are in separate
-- namespaces. In fact, it is possible to define a function foo, then define a
-- computation foo, and in the body of the computation foo, call the function
-- foo. See tests/backend/complex1.wpl, for example. It is also possible to
-- locally shadow a computation with a new binding; see
-- tests/backend/passfold-ifdead.wpl.
--
-- 3) When calling a computation, the arguments may either be expressions or
-- command_comp's. Both can start with "let <var_bind> = <exp>...".
--
-- One solution would be to eliminate syntactic distinctions between these
-- things and use types.

{------------------------------------------------------------------------------
 -
 - Identifiers
 -
 ------------------------------------------------------------------------------}

identifier :: { L String }
identifier :
    ID       { L (locOf $1) (unintern (getID $1)) }
  | 'arr'    { L (locOf $1) "arr" }
  | 'fun'    { L (locOf $1) "fun" }
  | 'length' { L (locOf $1) "length" }

comp_identifier :: { L String }
comp_identifier :
    ID       { L (locOf $1) (unintern (getID $1)) }
  | COMPID   { L (locOf $1) (unintern (getCOMPID $1)) }

{------------------------------------------------------------------------------
 -
 - Values
 -
 ------------------------------------------------------------------------------}

scalar_value :: { L Val }
scalar_value :
    'true'  { L (locOf $1) $ VBool True }
  | 'false' { L (locOf $1) $ VBool False }
  | "'0"    { L (locOf $1) $ VBit False }
  | "'1"    { L (locOf $1) $ VBit True }
  | '(' ')' { L (locOf $1) $ VUnit }
  | INT     { L (locOf $1) $ VInt (snd (getINT $1)) }
  | FLOAT   { L (locOf $1) $ VDouble (snd (getFLOAT $1)) }
  | STRING  { L (locOf $1) $ VString (snd (getSTRING $1)) }

{------------------------------------------------------------------------------
 -
 - Expressions
 -
 ------------------------------------------------------------------------------}

exp :: { SrcExp }
exp :
    exp_na           { $1 }
  | '{' exp_list '}' { eValArr (srclocOf $2) $2 }

exp_na :: { SrcExp }
exp_na :
    exp_na '+' exp_na
      { eBinOp ($1 `srcspan` $3) Add $1 $3 }
  | exp_na '-' exp_na
      { eBinOp ($1 `srcspan` $3) Sub $1 $3 }
  | exp_na '*' exp_na
      { eBinOp ($1 `srcspan` $3) Mult $1 $3 }
  | exp_na '/' exp_na
      { eBinOp ($1 `srcspan` $3) Div $1 $3 }
  | exp_na '%' exp_na
      { eBinOp ($1 `srcspan` $3) Rem $1 $3 }
  | exp_na '**' exp_na
      { eBinOp ($1 `srcspan` $3) Expon  $1 $3 }
  | exp_na '<<' exp_na
      { eBinOp ($1 `srcspan` $3) ShL $1 $3 }
  | exp_na '>>' exp_na
      { eBinOp ($1 `srcspan` $3) ShR $1 $3 }
  | exp_na '&' exp_na
      { eBinOp ($1 `srcspan` $3) BwAnd $1 $3 }
  | exp_na '|' exp_na
      { eBinOp ($1 `srcspan` $3) BwOr $1 $3 }
  | exp_na '^' exp_na
      { eBinOp ($1 `srcspan` $3) BwXor $1 $3 }
  | exp_na '==' exp_na
      { eBinOp ($1 `srcspan` $3) Eq $1 $3 }
  | exp_na '!=' exp_na
      { eBinOp ($1 `srcspan` $3) Neq $1 $3 }
  | exp_na '<' exp_na
      { eBinOp ($1 `srcspan` $3) Lt $1 $3 }
  | exp_na '>' exp_na
      { eBinOp ($1 `srcspan` $3) Gt $1 $3 }
  | exp_na '<=' exp_na
      { eBinOp ($1 `srcspan` $3) Leq $1 $3 }
  | exp_na '>=' exp_na
      { eBinOp ($1 `srcspan` $3) Geq $1 $3 }
  | exp_na '&&' exp_na
      { eBinOp ($1 `srcspan` $3) And $1 $3 }
  | exp_na '||' exp_na
      { eBinOp ($1 `srcspan` $3) Or $1 $3 }

  | 'length' exp_na %prec LENGTH
      { eUnOp' ($1 `srcspan` $2) ALength $2 }

  | '-' exp_na %prec NEG
      { eUnOp' ($1 `srcspan` $2) Neg $2 }
  | 'not' exp_na %prec NEG
      { eUnOp' ($1 `srcspan` $2) Not $2 }
  | '~' exp_na %prec NEG
      { eUnOp' ($1 `srcspan` $2) BwNeg $2 }

  | 'if' exp_na 'then' exp_na 'else' exp_na %prec IF
      { eIf ($1 `srcspan` $6) $2 $4 $6 }
  | 'if' exp_na 'then' exp_na 'else' error
      {% expected ["expression"] Nothing }
  | 'if' exp_na 'then' exp_na error
      {% expected ["else clause"] Nothing }
  | 'if' exp_na 'then' error
      {% expected ["expression"] Nothing }
  | 'if' exp_na error
      {% expected ["then clause"] Nothing }
  | let_decl 'in' exp_or_stmts %prec IF
      { eLetDecl $1 $3 }
  | let_decl error %prec IF
      {% expected ["'in'"] Nothing }

  | scalar_value
      { eValSrc (srclocOf $1) (unLoc $1) }
  | 'arr' '{' exp_list '}' { eValArr (srclocOf $3) $3 }
  -- XXX: we can shadow a comp
  | comp_identifier
      { mkVar (srclocOf $1) (unLoc $1) }
  | ID '{' struct_init_list1 '}'
      { eStruct' ($1 `srcspan` $4) (unintern (getID $1)) $3 }
  | struct_id '{' struct_init_list1 '}'
      { eStruct' ($1 `srcspan` $4) (unLoc $1) $3 }
  | ID derefs
      { $2 (mkVar (srclocOf $1) (unintern (getID $1))) }
  -- XXX: a fun comp can reuse a fun identifier, but the old fun identifier is
  -- still visible! The lexer has now way of telling this, of
  -- course... Disgusting!
  | comp_identifier '(' exp_list ')'
      { mkCall ($1 `srcspan` $4) (unLoc $1) $3 }
  | cast_type '(' exp_na ')'
      { eUnOp' ($1 `srcspan` $4) (Cast (unLoc $1)) $3 }
  | '(' exp ')'
      { $2 }
  | '(' exp error
      {% unclosed ($1 <--> $2) "(" }

exp_or_stmts :: { SrcExp }
exp_or_stmts :
    exp_na        { $1 }
  | '{' stmts '}' { $2 }

-- Variable binding
var_bind :: { SrcName }
var_bind :
    identifier
      { let { p = srclocOf $1
            ; i = unLoc $1
            }
        in
          toName i p SrcTyUnknown (errMutKind i p)
      }
  | '(' identifier ':' base_type ')'
      { let { p  = $1 `srcspan` $5
            ; i  = unLoc $2
            ; ty = unLoc $4
            }
        in
          toName i p ty (errMutKind i p)
      }
  -- XXX: we can shadow an existing comp
  |  COMPID
      { let { p = srclocOf $1
            ; i = unintern (getCOMPID $1)
            }
        in
          toName i p SrcTyUnknown (errMutKind i p)
      }

-- Constant integer expressions
const_int_exp :: { L Int }
const_int_exp :
    exp {% fmap (L (locOf $1)) (constIntExp $1) }

-- List of zero or more expressions
exp_list :: { [SrcExp] }
exp_list :
    exp_rlist { rev $1 }

exp_rlist :: { RevList SrcExp }
exp_rlist :
    {- empty -}       { rnil }
  | exp               { rsingleton $1 }
  | exp_rlist ',' exp { rcons $3 $1 }

-- List of one or more expressions
exp_list1 :: { [SrcExp] }
exp_list1 :
    exp_rlist1 { rev $1 }

exp_rlist1 :: { RevList SrcExp }
exp_rlist1 :
    exp                { rsingleton $1 }
  | exp_rlist1 ',' exp { rcons $3 $1 }

-- Struct initializers
struct_id :: { L String }
struct_id :
    ID          { L (locOf $1) (unintern (getID $1)) }
  -- XXX: a comp can shadow a struct def
  | COMPID      { L (locOf $1) (unintern (getCOMPID $1)) }
  | 'complex'   { L (locOf $1) "complex" }
  | 'complex8'  { L (locOf $1) "complex8" }
  | 'complex16' { L (locOf $1) "complex16" }
  | 'complex32' { L (locOf $1) "complex32" }
  | 'complex64' { L (locOf $1) "complex64" }

struct_init_list1 :: { [(String, SrcExp)] }
struct_init_list1 :
    struct_init1_rlist { rev $1 }

struct_init1_rlist :: { RevList (String, SrcExp) }
struct_init1_rlist :
    struct_init                        { rsingleton $1 }
  | struct_init1_rlist ';' struct_init { rcons $3 $1 }

struct_init :: { (String, SrcExp) }
struct_init :
    ID '=' exp { (unintern (getID $1), $3) }

-- One or more dereferences
derefs :: { SrcExp -> SrcExp }
derefs :
    deref_rlist { foldr1 (.) $1 }

-- We want these in reverse order for the foldr
deref_rlist :: { [SrcExp -> SrcExp] }
deref_rlist :
    deref             { [$1] }
  | deref_rlist deref { $2 : $1 }

deref :: { SrcExp -> SrcExp }
deref :
    '.' ID
      { \e -> eProj ($1 `srcspan` $2) e (unintern (getID $2)) }
  | range
      { \e -> eArrRead (srclocOf (fst $1)) e (fst $1) (snd $1) }

-- Ranges and intervals
range :: { (SrcExp, LengthInfo) }
range :
    '[' const_int_exp ':' const_int_exp ']'
      { let { p    = $1 `srcspan` $5
            ; from = unLoc $2
            ; to   = unLoc $4
            ; len  = to - from + 1
            }
        in
          (eValSrc p (vint from), LILength len)
      }
  | '[' exp ',' exp ']'
      {% case $4 of
           { MkExp (EVar (MkName { name = n })) _ _ -> return ($2, LIMeta n)
           ; e -> do { len <- constIntExp e
                     ; return ($2, LILength len)
                     }
           }
      }
  | '[' exp ']'
      { ($2, LISingleton) }

gen_interval :: { (SrcExp, SrcExp) }
gen_interval :
    '[' const_int_exp ':' const_int_exp ']'
      { let { p    = $1 `srcspan` $5
            ; from = unLoc $2
            ; to   = unLoc $4
            ; len  = to - from + 1
            }
        in
          (eValSrc p (vint from), eValSrc p (vint len))
      }
  | '[' exp ',' exp ']'
      { ($2, $4) }

{------------------------------------------------------------------------------
 -
 - Types
 -
 ------------------------------------------------------------------------------}

base_type :: { L SrcTy }
base_type :
    '(' ')'            { L ($1 <--> $2) $ SrcTUnit }
  | 'bit'              { L (locOf $1)   $ SrcTBit }
  | 'int'              { L (locOf $1)   $ SrcTInt SrcBW32 }
  | 'int8'             { L (locOf $1)   $ SrcTInt SrcBW8 }
  | 'int16'            { L (locOf $1)   $ SrcTInt SrcBW16 }
  | 'int32'            { L (locOf $1)   $ SrcTInt SrcBW32 }
  | 'int64'            { L (locOf $1)   $ SrcTInt SrcBW64 }
  | 'double'           { L (locOf $1)   $ SrcTDouble }
  | 'bool'             { L (locOf $1)   $ SrcTBool }
  | 'complex'          { L (locOf $1)   $ SrcTStruct complex32TyName }
  | 'complex8'         { L (locOf $1)   $ SrcTStruct complex8TyName }
  | 'complex16'        { L (locOf $1)   $ SrcTStruct complex16TyName }
  | 'complex32'        { L (locOf $1)   $ SrcTStruct complex32TyName }
  | 'complex64'        { L (locOf $1)   $ SrcTStruct complex64TyName }
  | 'struct' struct_id { L ($1 <--> $2) $ SrcTStruct (unLoc $2) }
  | 'arr' arr_length   { L ($1 <--> $2) $ unLoc $2 }
  | '(' base_type ')'  { L ($1 <--> $3) $ unLoc $2 }

cast_type :: { L SrcTy }
cast_type :
    'bit'             { L (locOf $1)   $ SrcTBit }
  | 'int'             { L (locOf $1)   $ SrcTInt SrcBW32 }
  | 'int8'            { L (locOf $1)   $ SrcTInt SrcBW8 }
  | 'int16'           { L (locOf $1)   $ SrcTInt SrcBW16 }
  | 'int32'           { L (locOf $1)   $ SrcTInt SrcBW32 }
  | 'int64'           { L (locOf $1)   $ SrcTInt SrcBW64 }
  | 'double'          { L (locOf $1)   $ SrcTDouble }
  | 'complex'         { L (locOf $1)   $ SrcTStruct complex32TyName }
  | 'complex8'        { L (locOf $1)   $ SrcTStruct complex8TyName }
  | 'complex16'       { L (locOf $1)   $ SrcTStruct complex16TyName }
  | 'complex32'       { L (locOf $1)   $ SrcTStruct complex32TyName }
  | 'complex64'       { L (locOf $1)   $ SrcTStruct complex64TyName }

arr_length :: { L SrcTy }
arr_length :
    '[' 'length' '(' ID ')' ']' base_type
      { let { p  = $1 `srcspan` $7
            ; x  = unintern (getID $4)
            ; nm = toName x p SrcTyUnknown (errMutKind x p)
            }
        in
          L ($1 <--> $7) $ SrcTArray (SrcNArr nm) (unLoc $7)
      }
  | '[' const_int_exp ']' base_type
      { L ($1 <--> $4) $ SrcTArray (SrcLiteral (unLoc $2)) (unLoc $4) }
  | base_type
      { let { p = srclocOf $1 }
        in
          L (locOf $1) $ SrcTArray (SrcNVar p) (unLoc $1)
      }

comp_base_type :: { L SrcCTy }
comp_base_type :
    'ST' index_type base_type base_type
      { L ($1 <--> $4) $ $2 (unLoc $3) (unLoc $4) }
  | '(' comp_base_type ')'
      { L ($1 <--> $3) $ unLoc $2 }

index_type :: { SrcTy -> SrcTy -> SrcCTy }
index_type :
    'T'                { \ti ty -> SrcCTyKnown (CTTrans ti ty) }
  | 'C' base_type      { \ti ty -> SrcCTyKnown (CTComp (unLoc $2) ti ty) }
  | '(' index_type ')' { $2 }

{------------------------------------------------------------------------------
 -
 - Declarations
 -
 ------------------------------------------------------------------------------}

let_decl :: { ELetDecl }
let_decl :
    decl
      { let { p      = srclocOf $1
            ; (x, e) = unLoc $1
            }
        in
          ELetDeclERef p x e
      }
  | 'let' var_bind '=' exp
      { let { p = $1 `srcspan` $4
            ; x = $2
            ; e = $4
            }
        in
          ELetDeclExpr p x e
      }
  | 'let' var_bind error
      {% expected ["'='"] Nothing }

decl :: { L (SrcName, Maybe SrcExp) }
decl :
    'var' ID ':' base_type maybe_initializer
      { let { p   = $1 `srcspan` $5
            ; tau = unLoc $4
            ; x   = unintern (getID $2)
            }
        in
          L ($1 <--> $5) (toName x p tau (errMutKind x p), unLoc $5)
      }

maybe_initializer :: { L (Maybe SrcExp) }
maybe_initializer :
    {- empty -} { L NoLoc        Nothing }
  | ':=' exp    { L ($1 <--> $2) (Just $2) }

{------------------------------------------------------------------------------
 -
 - Statements
 -
 ------------------------------------------------------------------------------}

stmt_block :: { SrcExp }
stmt_block :
    '{' stmts '}' { $2 }
  | stmt_exp      { $1 }

stmts :: { SrcExp }
stmts :
    stmt_rlist opt_semi {% desugarStatements (rev $1) }

stmt_rlist :: { RevList Statement }
stmt_rlist :
    stmt                     { rsingleton $1 }
  | stmt_rlist opt_semi stmt { rcons $3 $1 }

stmt :: { Statement }
stmt :
    let_decl
      { SLetDecl $1 }
  | stmt_exp
      { SExp $1 }

stmt_exp :: { SrcExp }
stmt_exp :
    unroll_info 'for' var_bind 'in' gen_interval stmt_block
      { let { p              = $1 `srcspan` $6
            ; ui             = unLoc $1
            ; k              = $3
            ; (estart, elen) = $5
            ; body           = $6
            }
        in
          eFor p ui k estart elen body
      }
  | 'while' '(' exp ')' stmt_block
      { let { p = $1 `srcspan` $5 }
        in
          eWhile p $3 $5
      }
  | 'if' exp 'then' stmt_block 'else' stmt_block
      { let { p = $1 `srcspan` $6 }
        in
          eIf p $2 $4 $6
      }
  | 'if' exp 'then' stmt_block 'else' error
      {% expected ["statement block"] Nothing }
  | 'if' exp 'then' stmt_block
      { let { p = $1 `srcspan` $4 }
        in
          eIf p $2 $4 (eUnit p)
      }
  | 'if' exp error
      {% expected ["then clause"] Nothing }
  | 'return' exp
      { $2 }
  | 'print' exp_list1
      { ePrint ($1 `srcspan` $2) False $2 }
  | 'println' exp_list1
      { ePrint ($1 `srcspan` $2) True $2 }
  | 'error' STRING
      { let { p = $1 `srcspan` $2 }
        in
          eError p SrcTyUnknown (snd (getSTRING $2))
      }
  | let_decl 'in' stmt_block
      { eLetDecl $1 $3 }
  | let_decl error
      {% expected ["'in'"] Nothing }
  | ID '(' exp_list ')'
      { let { p = ($1 `srcspan` $3)
            ; x = unintern (getID $1)
            }
        in
          mkCall p x $3
      }
  | ID projs ':=' exp
      { let { p    = $1 `srcspan` $4
            ; x    = unintern (getID $1)
            ; proj = $2
            ; rhs  = $4
            }
        in
          eAssign p (proj (mkVar p x)) rhs
      }
  | ID projs range ':=' exp
      { let { p             = $1 `srcspan` $5
            ; x             = unintern (getID $1)
            ; proj          = $2
            ; (estart, len) = $3
            ; rhs           = $5
            }
        in
          eArrWrite p (proj (mkVar p x)) estart len rhs
      }

unroll_info :: { L UnrollInfo }
unroll_info :
    {- empty -} { L NoLoc      AutoUnroll }
  | 'unroll'    { L (locOf $1) Unroll }
  | 'nounroll'  { L (locOf $1) NoUnroll }

projs :: { SrcExp -> SrcExp }
projs :
    proj_rlist { foldr (.) id $1 }

-- We want these in reverse order for the foldr
proj_rlist :: { [SrcExp -> SrcExp] }
proj_rlist :
    {- empty -}         { [] }
  | proj_rlist '.' proj { $3 : $1 }

proj :: { SrcExp -> SrcExp }
proj :
    ID
      { let { p = srclocOf $1
            ; v = unintern (getID $1)
            }
        in
          \e -> eProj p e v
      }

{------------------------------------------------------------------------------
 -
 - Computation Expressions
 -
 ------------------------------------------------------------------------------}

commands :: { SrcComp }
commands :
    command_rlist opt_semi {% desugarCommands (rev $1) }

command_rlist :: { RevList Command }
command_rlist :
    command                        { rsingleton $1 }
  | command_rlist opt_semi command { rcons $3 $1 }

command :: { Command }
command :
    comp_let_decl
      { CDecl $1 }
  | var_bind '<-' command_comp
      { let { p  = $1 `srcspan` $3
            ; x  = $1
            ; c1 = $3
            }
        in
          CBind p x c1
      }
  | command_comp
      { CComp $1 }

comp_let_decl :: { CLetDecl }
comp_let_decl :
    decl
      { let { p      = srclocOf $1
            ; (x, e) = unLoc $1
            }
        in
          CLetDeclERef p x e
      }
  | struct
      { let { p = srclocOf $1 }
        in
          CLetDeclStruct p (unLoc $1)
      }
  | 'fun' 'external' ID params ':' base_type
      { let { p  = $1 `srcspan` $6
            ; x  = unintern (getID $3)
            ; ps = $4
            ; ty = unLoc $6
            }
        in
          CLetDeclExternal p x ps ty
      }
  | 'fun' 'comp' maybe_comp_range comp_var_bind comp_params '{' commands '}'
      { let { p  = $1 `srcspan` $6
            ; h  = $3
            ; x  = $4
            ; ps = $5
            ; c  = $7
            }
        in
          CLetDeclFunComp p h x ps c
      }
  | 'fun' var_bind params stmt_block
      { let { p  = $1 `srcspan` $4
            ; fn = $2
            ; ps = $3
            ; e  = $4
            }
        in
          CLetDeclFunExpr p fn ps e
      }
  | 'let' 'comp' maybe_comp_range comp_var_bind '=' command_comp
      { let { p  = $1 `srcspan` $6
            ; h  = $3
            ; x  = $4
            ; c  = $6
            }
        in
          CLetDeclComp p h x c
      }
  | 'let' var_bind '=' exp
      { let { p  = $1 `srcspan` $4
            ; x  = $2
            ; e  = $4
            }
        in
          CLetDeclExpr p x e
      }

command_comp :: { SrcComp }
command_comp :
    'standalone' command_comp %prec STANDALONE
      { cStandalone ($1 `srcspan` $2) $2 }
  | 'repeat' vect_ann command_comp %prec STANDALONE
      { cRepeat ($1 `srcspan` $3) $2 $3 }
  | 'until' exp command_comp %prec STANDALONE
      { cUntil ($1 `srcspan` $3) $2 $3 }
  | 'while' exp command_comp %prec STANDALONE
      { cWhile ($1 `srcspan` $3) $2 $3 }
  | unroll_info 'times' exp command_comp %prec STANDALONE
      { let { p  = $1 `srcspan` $3
            ; ui = unLoc $1
            ; e  = $3
            ; c  = $4
            ; nm = toName "_tmp_count" p tintSrc Imm
            }
        in
          cTimes p ui (eVal p tintSrc (VInt 0)) e nm c
      }
  | unroll_info 'for' var_bind 'in' gen_interval command_comp %prec STANDALONE
      { let { p              = $1 `srcspan` $6
            ; ui             = unLoc $1
            ; k              = $3
            ; (estart, elen) = $5
            ; c              = $6
            }
        in
          cTimes p ui estart elen k c
      }
  | command_comp '>>>' command_comp
      { cPar ($1 `srcspan` $3) (mkParInfo MaybePipeline) $1 $3 }
  | command_comp '|>>>|' command_comp
      { cPar ($1 `srcspan` $3) (mkParInfo (AlwaysPipeline 0 0)) $1 $3 }
  | command_comp error
      {% expected [">>> or |>>>|"] Nothing }
  | comp_term
      { $1 }

comp_term :: { SrcComp }
comp_term :
    '(' command_comp ')'
      { $2 }
  | inline_ann 'return' exp
      { cReturn ($1 `srcspan` $3) (unLoc $1) $3 }
  | 'emit' exp
      { cEmit ($1 `srcspan` $2) $2 }
  | 'emits' exp
      { cEmits ($1 `srcspan` $2) $2 }
  | 'take'
      { cTake1 (srclocOf $1) SrcTyUnknown }
  | 'takes' exp
      {% let { p = $1 `srcspan` $2 }
         in
           case evalSrcInt $2 of
             { (Right n, _) -> return $ cTake p SrcTyUnknown (fromInteger n)
             ; (Left  _, _) -> fail "Non-constant argument to takes"
             }
      }
  | 'filter' var_bind
      { cFilter (srclocOf $1) $2 }
  | 'read' type_ann
      { cReadSrc ($1 `srcspan` $2) (unLoc $2) }
  | 'write' type_ann
      { cWriteSnk ($1 `srcspan` $2) (unLoc $2) }
  | 'map' vect_ann var_bind
      { cMap (srclocOf $1) $2 $3 }
  | 'if' exp 'then' command_comp comp_maybe_else
      { let { sloc = $1 `srcspan` $5 }
        in
          cBranch sloc $2 $4 (unLoc $5 sloc)
      }
  | 'if' exp 'then' error
      {% expected ["command"] Nothing }
  | 'if' exp ';'
      {% expected ["then clause"] Nothing }
  | comp_let_decl 'in' command_comp
      { cLetDecl $1 $3 }
  | comp_let_decl error
      {% expected ["'in'"] Nothing }
  | 'do' stmt_block
      { cReturn ($1 `srcspan` $2) AutoInline $2 }
  | 'seq' '{' commands '}'
      { $3 }
  | '{' commands '}'
      { $2 }
  | COMPID
      { let { p = srclocOf $1
            ; x = unintern (getCOMPID $1)
            ; v = toName x p SrcCTyUnknown (errMutKind x p)
            }
        in
          cVar p v
      }
  | COMPID '(' args ')'
      { let { p = srclocOf $1
            ; x = unintern (getCOMPID $1)
            ; v = toName x p SrcCTyUnknown (errMutKind x p)
            }
        in
          cCall p v $3
      }

comp_maybe_else :: { L (SrcLoc -> SrcComp) }
comp_maybe_else :
    {- empty -}         { L NoLoc        cUnit }
  | 'else' command_comp { L ($1 <--> $2) (\_ -> $2) }

inline_ann :: { L ForceInline }
inline_ann :
    {- empty -}   { L NoLoc      AutoInline }
  | 'noinline'    { L (locOf $1) NoInline }
  | 'forceinline' { L (locOf $1) ForceInline }
  | 'autoinline'  { L (locOf $1) AutoInline }

type_ann :: { L SrcTy }
type_ann :
    {- empty -}       { L NoLoc SrcTyUnknown }
  | '[' base_type ']' { $2 }

vect_ann :: { Maybe VectAnn }
vect_ann :
    {- empty -}        { Nothing }
  | vect_ann_flag      { Just (uncurry Rigid $1) }
  | '<=' vect_ann_flag { Just (uncurry UpTo $2) }

vect_ann_flag :: { (Bool, (Int, Int)) }
vect_ann_flag :
    comp_range     { (True,  $1) }
  | '!' comp_range { (False, $2) }

-- Computation function arguments
args :: { [CallArg SrcExp SrcComp] }
args :
    arg_rlist { rev $1 }

arg_rlist :: { RevList (CallArg SrcExp SrcComp) }
arg_rlist :
    {- empty -}       { rnil }
  | arg               { rsingleton $1 }
  | arg_rlist ',' arg { rcons $3 $1 }

arg :: { CallArg SrcExp SrcComp }
arg :
    exp          { CAExp $1 }
  | command_comp { CAComp $1 }

-- Comp ranges
comp_range :: { (Int, Int) }
comp_range :
    '[' INT ',' INT ']' { (fromIntegral (snd (getINT $2)), fromIntegral (snd (getINT $4))) }

maybe_comp_range :: { Maybe (Int, Int) }
maybe_comp_range :
    {- empty -} { Nothing }
  | comp_range  { Just $1 }

-- Comp variable binding
comp_var_bind :: { GName SrcCTy }
comp_var_bind :
    comp_identifier
      {% let { p = srclocOf $1
             ; i = unLoc $1
             }
         in
           do { addCompIdentifier (intern i)
              ; return $ toName i p SrcCTyUnknown (errMutKind i p)
              }
      }
  | '(' comp_identifier ':' comp_base_type ')'
      {% let { p  = $1 `srcspan` $5
             ; i  = unLoc $2
             ; ty = unLoc $4
             }
         in
           do { addCompIdentifier (intern i)
              ; return $ toName i p ty (errMutKind i p)
              }
      }

-- structs
struct :: { L SrcStructDef }
struct :
    'struct' ID '=' '{' field_list '}'
      { L ($1 <--> $6) $ StructDef (unintern (getID $2)) $5 }

field_list :: { [(String, SrcTy)] }
field_list :
    field_rlist { rev $1 }

field_rlist :: { RevList (String, SrcTy) }
field_rlist :
    {- empty -}           { rnil }
  | field                 { rsingleton $1 }
  | field_rlist ';' field { rcons $3 $1 }

field :: { (String, SrcTy) }
field :
    ID ':' base_type
      { (unintern (getID $1), unLoc $3) }

-- Parameters to a (non-comp) function
params :: { [SrcName] }
params :
    '(' param_rlist ')' { rev $2 }

param_rlist :: { RevList SrcName }
param_rlist :
    {- empty -}           { rnil }
  | param                 { rsingleton $1 }
  | param_rlist ',' param { rcons $3 $1 }

param :: { SrcName }
param :
    'var' ID ':' base_type
      { let { p  = $1 `srcspan` $4
            ; x  = unintern (getID $2)
            ; ty = unLoc $4
            }
        in
          toName x p ty Mut
      }
  | identifier ':' base_type
      { let { p  = $1 `srcspan` $3
            ; x  = unLoc $1
            ; ty = unLoc $3
            }
        in
          toName x p ty Imm
      }

-- Parameters to a comp function
comp_params :: { [GName (CallArg SrcTy SrcCTy)] }
comp_params :
    '(' comp_param_rlist ')' { rev $2 }

comp_param_rlist :: { RevList (GName (CallArg SrcTy SrcCTy)) }
comp_param_rlist :
    {- empty -}                     { rnil }
  | comp_param                      { rsingleton $1 }
  | comp_param_rlist ',' comp_param { rcons $3 $1 }

comp_param :: { GName (CallArg SrcTy SrcCTy) }
comp_param :
    'var' ID ':' base_type
      { let { p  = $1 `srcspan` $4
            ; x  = unintern (getID $2)
            ; ty = unLoc $4
            }
        in
          toName x p (CAExp ty) Mut
      }
  | 'var' ID ':' comp_base_type
      {% fail "Computation parameter cannot be mutable" }
  | ID ':' base_type
      { let { p  = $1 `srcspan` $3
            ; x  = unintern (getID $1)
            ; ty = unLoc $3
            }
        in
          toName x p (CAExp ty) Imm
      }
  | ID ':' comp_base_type
      {% let { p  = $1 `srcspan` $3
             ; x  = unintern (getID $1)
             ; ty = unLoc $3
             }
         in
           do { addCompIdentifier (getID $1)
              ; return $ toName x p (CAComp ty) Imm
              }
      }
  | ID ':' error
      {% expected ["'ST' or base type"] Nothing }

{------------------------------------------------------------------------------
 -
 - Programs
 -
 ------------------------------------------------------------------------------}

program :: { SrcProg }
program :
    comp_let_decl_rlist opt_semi
      {% do { let decls = rev $1
            ; c <- extractMain [] decls >>= desugarCommands
            ; return (MkProg c)
            }
      }

comp_let_decl_rlist :: { RevList CLetDecl }
comp_let_decl_rlist :
    comp_let_decl                              { rsingleton $1 }
  | comp_let_decl_rlist opt_semi comp_let_decl { rcons $3 $1 }

{------------------------------------------------------------------------------
 -
 - Miscellaneous
 -
 ------------------------------------------------------------------------------}

opt_semi :: { () }
opt_semi :
    {- empty -} { () }
  | ';'         { () }

{
happyError :: L T.Token -> P a
happyError (L loc t) =
    parserError (locStart loc) (text "parse error on" <+> quote (ppr t))
  where
    quote :: Doc -> Doc
    quote = enclose (char '`') (char '\'')

getINT         (L _ (T.TintConst x))           = x
getFLOAT       (L _ (T.TfloatConst x))         = x
getCHAR        (L _ (T.TcharConst x))          = x
getSTRING      (L _ (T.TstringConst x))        = x
getID          (L _ (T.Tidentifier ident))     = ident
getCOMPID      (L _ (T.TcompIdentifier ident)) = ident

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

type SrcName = GName SrcTy

type SrcCName = GName SrcCTy

type SrcStructDef = GStructDef SrcTy

errMutKind :: String -> SrcLoc -> MutKind
errMutKind s loc =
    error $ "MutKind missing: " ++ s ++ ", " ++ displayLoc (locOf loc)

mkVar :: SrcLoc -> String -> SrcExp
mkVar p x = eVar p (toName x p SrcTyUnknown (errMutKind x p))

mkCall :: SrcLoc -> String -> [SrcExp] -> SrcExp
mkCall p fn args = eCall p (toName fn p SrcTyUnknown Imm) args

mkVectComp :: SrcComp -> Maybe (Int, Int) -> SrcComp
mkVectComp sc Nothing  = sc
mkVectComp sc (Just h) = cVectComp (compLoc sc) h sc

eUnit :: SrcLoc -> SrcExp
eUnit p = eValSrc p VUnit

cUnit :: SrcLoc -> SrcComp
cUnit p = cReturn p ForceInline (eUnit p)

eValSrc :: SrcLoc -> Val -> SrcExp
eValSrc p v = eVal p SrcTyUnknown v

eUnOp' :: SrcLoc -> GUnOp t -> GExp t () -> GExp t ()
eUnOp' p op e =
    case (op, unExp e) of
      (Neg, EVal ty (VInt i)) -> eVal eloc ty (VInt (negate i))
      _                       -> eUnOp p op e
  where
    eloc = expLoc e

eStruct' :: SrcLoc -> String -> [(String, SrcExp)] -> SrcExp
eStruct' loc "complex" = eStruct loc (SrcTStruct complex32TyName)
eStruct' loc tn        = eStruct loc (SrcTStruct tn)

constIntExp :: SrcExp -> P Int
constIntExp e =
    case evalSrcInt e of
      (Right i, _) -> return $ fromIntegral i
      (Left _,  _) -> failAt (locOf e) "Non-constant array length expression."

extractMain :: [CLetDecl] -> [CLetDecl] -> P [Command]
extractMain _   []     = fail "No main found"
extractMain acc (d:ds) =
  case d of
    CLetDeclComp _p Nothing nm c | name nm == "main" ->
      return $ map CDecl (reverse acc ++ ds) ++ [CComp c]
    _ ->
      extractMain (d:acc) ds

-- | Local declarations
--
-- This is the equivalent of 'CLetDecl' in the comp parser.
data ELetDecl = ELetDeclERef SrcLoc (GName SrcTy) (Maybe SrcExp)
              | ELetDeclExpr SrcLoc (GName SrcTy) SrcExp

instance Located ELetDecl where
    locOf (ELetDeclERef p _ _) = locOf p
    locOf (ELetDeclExpr p _ _) = locOf p

eLetDecl :: ELetDecl -> SrcExp -> SrcExp
eLetDecl (ELetDeclERef p x e) = eLetRef p x e
eLetDecl (ELetDeclExpr p x e) = eLet p x AutoInline e

eletDeclName :: ELetDecl -> String
eletDeclName (ELetDeclERef _ x _) = show x
eletDeclName (ELetDeclExpr _ x _) = show x

-- | Statements
--
-- This is the equivalent to 'Command' in the comp parser.
data Statement = SLetDecl ELetDecl
               | SExp SrcExp

desugarStatements :: [Statement] -> P SrcExp
desugarStatements [] =
    error "desugarStatements: empty list"

desugarStatements [SLetDecl d] =
    failAt (locOf d) $
           unlines [ "A block must end on an expression, it cannot end on a declaration."
                   , "The declaration for " ++ show (eletDeclName d) ++ " appears unused?"
                   ]

desugarStatements [SExp e] =
    return e

desugarStatements (SExp e1 : ss) = do
    e2 <- desugarStatements ss
    return $ eSeq (expLoc e2) e1 e2

desugarStatements (SLetDecl d : ss) =
    eLetDecl d <$> desugarStatements ss

-- | Declarations
--
-- This is the equivalent of `ELetDecl` and co in the expression parser.
data CLetDecl = CLetDeclERef     SrcLoc SrcName (Maybe SrcExp)
              | CLetDeclStruct   SrcLoc SrcStructDef
              | CLetDeclExternal SrcLoc String [SrcName] SrcTy
              | CLetDeclFunComp  SrcLoc (Maybe (Int, Int)) SrcCName [GName (CallArg SrcTy SrcCTy)] SrcComp
              | CLetDeclFunExpr  SrcLoc SrcName [SrcName] SrcExp
              | CLetDeclComp     SrcLoc (Maybe (Int, Int)) SrcCName SrcComp
              | CLetDeclExpr     SrcLoc SrcName SrcExp

instance Located CLetDecl where
    locOf (CLetDeclERef p _ _)        = locOf p
    locOf (CLetDeclStruct p _)        = locOf p
    locOf (CLetDeclExternal p _ _ _)  = locOf p
    locOf (CLetDeclFunComp p _ _ _ _) = locOf p
    locOf (CLetDeclFunExpr p _ _ _)   = locOf p
    locOf (CLetDeclComp p _ _ _)      = locOf p
    locOf (CLetDeclExpr p _ _)        = locOf p

cLetDecl :: CLetDecl -> SrcComp -> SrcComp
cLetDecl (CLetDeclStruct   p sdef)     = cLetStruct p sdef
cLetDecl (CLetDeclERef     p x e)      = cLetERef   p x e
cLetDecl (CLetDeclExpr     p x e)      = cLetE      p x AutoInline e
cLetDecl (CLetDeclComp     p h x c)    = cLet       p x    (mkVectComp c h)
cLetDecl (CLetDeclFunComp  p h x ps c) = cLetFunC   p x ps (mkVectComp c h)
cLetDecl (CLetDeclFunExpr  p fn ps e)  = cLetHeader p (mkFunDefined  p fn ps e)
cLetDecl (CLetDeclExternal p x ps ty)  = cLetHeader p (mkFunExternal p fn ps ty)
  where
    fn = toName x p SrcTyUnknown (errMutKind x p)

cLetDeclName :: CLetDecl -> String
cLetDeclName (CLetDeclERef _ nm _)        = show nm
cLetDeclName (CLetDeclStruct _ sdef)      = struct_name sdef
cLetDeclName (CLetDeclExternal _ nm _ _)  = nm
cLetDeclName (CLetDeclFunComp _ _ nm _ _) = show nm
cLetDeclName (CLetDeclFunExpr _ nm _ _)   = show nm
cLetDeclName (CLetDeclComp _ _ nm _)      = show nm
cLetDeclName (CLetDeclExpr _ nm _)        = show nm

-- | Commands
--
-- This is the equivalent to 'Statement' in the expression parser.
data Command = CDecl CLetDecl
             | CBind SrcLoc (GName SrcTy) SrcComp
             | CComp SrcComp

instance Located Command where
    locOf (CDecl d)     = locOf d
    locOf (CBind p _ _) = locOf p
    locOf (CComp c)     = locOf c

desugarCommands :: [Command] -> P SrcComp
desugarCommands [] =
    error "desugarCommands: empty list"

desugarCommands [CDecl d] =
    failAt (locOf d) $
    unlines [ "A block must end on a computation, it cannot end on a declaration."
            , "The declaration for " ++ show (cLetDeclName d) ++ " appears unused?"
            ]

desugarCommands [CBind p x _] =
    failAt (locOf p) $
    unlines [ "A block must end on a computation, it cannot end on a bind."
            , "Try removing the '" ++ show x ++ " <-' part perhaps?"
            ]

desugarCommands [CComp c] =
    return c

desugarCommands (CDecl d:cs) =
    cLetDecl d <$> desugarCommands cs

desugarCommands (CBind p x c1:cs) = do
    c2 <- desugarCommands cs
    return $ cBindMany p c1 [(x, c2)]

desugarCommands (CComp c1:cs) = do
    c2 <- desugarCommands cs
    return $ cSeq (compLoc c2) c1 c2

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data RevList a  =  RNil
                |  RCons a (RevList a)
                |  RApp [a] (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

infixr 5 `rcons`

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rapp :: [a] -> RevList a -> RevList a
rapp xs ys  = RApp xs ys

rlist :: [a] -> RevList a
rlist xs = rlist' xs rnil
  where
    rlist' []     acc = acc
    rlist' (x:xs) acc = rlist' xs (rcons x acc)

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
    go  l  (RApp xs ys)  = go (xs ++ l) ys

instance Located a => Located (RevList a) where
    locOf RNil         = mempty
    locOf (RCons x xs) = locOf x `mappend` locOf xs
    locOf (RApp xs ys) = locOf xs `mappend` locOf ys
}
