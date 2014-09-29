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
{-# LANGUAGE GADTs, TemplateHaskell,
             MultiParamTypeClasses,
             RankNTypes,
             TypeSynonymInstances,
             FlexibleInstances,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module BlinkParseExpr (
    -- * Top-level parsers
    parseExpr
  , declParser
  , parseBaseType
  , parseStmtBlock
  , parseStmts
    -- * Utilities
  , parseFor
  , mkNameFromPos
  , parseVarBind
  , genIntervalParser
  , eunit
  ) where

import Control.Applicative ((<*>), (<$), (<*), (*>), (<$>))
import Control.Monad (join)
import Data.Maybe ( fromMaybe, fromJust )
import Text.Parsec
import Text.Parsec.Expr

import AstExpr
import BlinkLexer
import BlinkParseM
import Eval ( evalInt )

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Expressions
--
-- > <expr> ::=
-- >     "-"      <expr>     -- negation
-- >   | "not"    <expr>     -- not
-- >   | "~"      <expr>     -- bitwise negation
-- >
-- >   | "length" <expr>     -- length
-- >
-- >   | <expr> "**" <expr>  -- exponentiation
-- >   | <expr> "*"  <expr>  -- multiplication
-- >   | <expr> "/"  <expr>  -- division
-- >   | <expr> "%"  <expr>  -- remainder
-- >
-- >   | <expr> "+"  <expr>  -- addition
-- >   | <expr> "-"  <expr>  -- subtraction
-- >
-- >   | <expr> "<<" <expr>  -- shift left
-- >   | <expr> ">>" <expr>  -- shift right
-- >
-- >   | <expr> "<"  <expr>  -- less than
-- >   | <expr> "<=" <expr>  -- less than or equal to
-- >   | <expr> ">"  <expr>  -- greater than
-- >   | <expr> ">=" <expr>  -- greater than or equal to
-- >
-- >   | <expr> "&"  <expr>  -- bitwise AND
-- >
-- >   | <expr> "^"  <expr>  -- bitwise XOR
-- >
-- >   | <expr> "|"  <expr>  -- bitwise OR
-- >
-- >   | <expr> "==" <expr>  -- equality
-- >   | <expr> "!=" <expr>  -- inequality
-- >
-- >   | <expr> "&&" <expr>  -- logical AND
-- >   | <expr> "||" <expr>  -- logical OR
-- >   | <term>
--
-- where we have grouped operators with the same precedence, and groups of
-- operators listed earlier have higher precedence.
parseExpr :: BlinkParser SrcExp
parseExpr =
    buildExpressionParser table parseTerm <?> "expression"
  where
    table =
      [
        [ Prefix $ withPos eUnOp  <*> (Neg     <$ reservedOp "-")
        , Prefix $ withPos eUnOp  <*> (Not     <$ reserved   "not")
        , Prefix $ withPos eUnOp  <*> (BwNeg   <$ reservedOp "~")
        ]

      , [ Prefix $ withPos eUnOp  <*> (ALength <$ reserved   "length") ]

      , [ lInfix $ withPos eBinOp <*> (Expon   <$ reservedOp "**")
        , lInfix $ withPos eBinOp <*> (Mult    <$ reservedOp "*")
        , lInfix $ withPos eBinOp <*> (Div     <$ reservedOp "/")
        , lInfix $ withPos eBinOp <*> (Rem     <$ reservedOp "%")
        ]

      , [ lInfix $ withPos eBinOp <*> (Add     <$ reservedOp "+" )
        , lInfix $ withPos eBinOp <*> (Sub     <$ reservedOp "-" )
        ]

      , [ lInfix $ withPos eBinOp <*> (ShL     <$ reservedOp "<<")
        , lInfix $ withPos eBinOp <*> (ShR     <$ reservedOp ">>")
        ]

      , [ lInfix $ withPos eBinOp <*> (Lt      <$ reservedOp "<" )
        , lInfix $ withPos eBinOp <*> (Leq     <$ reservedOp "<=")
        , lInfix $ withPos eBinOp <*> (Gt      <$ reservedOp ">" )
        , lInfix $ withPos eBinOp <*> (Geq     <$ reservedOp ">=")
        ]

      , [ lInfix $ withPos eBinOp <*> (BwAnd   <$ reservedOp "&" ) ]

      , [ lInfix $ withPos eBinOp <*> (BwXor   <$ reservedOp "^" ) ]

      , [ lInfix $ withPos eBinOp <*> (BwOr    <$ reservedOp "|" )  ]

      , [ lInfix $ withPos eBinOp <*> (Eq      <$ reservedOp "==")
        , lInfix $ withPos eBinOp <*> (Neq     <$ reservedOp "!=")
        ]

      , [ lInfix $ withPos eBinOp <*> (And     <$ reservedOp "&&") ]
      , [ lInfix $ withPos eBinOp <*> (Or      <$ reservedOp "||")  ]
      ]

-- | Terms in the expression
--
-- > <term> ::=
-- >     "(" <expr> ")"
-- >   | "()"
-- >   | <value>
-- >   | IDENT "{" (IDENT "=" <expr>)*";")     -- struct init
-- >   | IDENT ("." IDENT | "[" <range> "]")*  -- struct or array index
-- >   | IDENT "(" <expr>*"," ")"              -- function call or cast
-- >   | IDENT                                 -- variable
-- >   | "let" <var-bind> "=" <expr> "in" <expr>
-- >   | <decl> "in" <expr>
-- >   | "if" <expr> "then" <expr> "else" <expr>
parseTerm :: BlinkParser SrcExp
parseTerm = choice
    [ parens $ choice [ try parseExpr
                      , withPos eVal <*> (VUnit <$ whiteSpace)
                      ]
    , parseValue
    , parseWithVarOnHead
    , withPos eLet' <* reserved "let" <*> parseVarBind
                    <* symbol "="     <*> parseExpr
                    <* reserved "in"  <*> parseExpr
    , withPos eLetRef' <*> declParser <* reserved "in" <*> parseExpr
    , withPos eIf <* reserved "if"   <*> parseExpr
                  <* reserved "then" <*> parseExpr
                  <* reserved "else" <*> parseExpr
    ] <?> "expression"
  where
    eLet'    p () x          = eLet    p () x AutoInline
    eLetRef' p () (x, ty, e) = eLetRef p () x ty e

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Values
--
-- > <value> ::= <scalar-value> | "{" <scalar-value>*"," "}"
parseValue :: BlinkParser SrcExp
parseValue = choice
    [ withPos eVal    <*> parseScalarValue
    , withPos eValArr <*> braces (sepBy parseScalarValue comma)
    ] <?> "value"

-- | Scalar values
--
-- > <scalar-value> ::=
-- >     "(" <scalar-value> ")"
-- >   | "true"
-- >   | "false"
-- >   | "'0"
-- >   | "'1"
-- >   | "()"
-- >   | FLOAT
-- >   | STRING
-- >   | INT
--
-- TODO: We currently allow for whitespace for the unit value. Is that
-- really what we want?
parseScalarValue :: BlinkParser Val
parseScalarValue = choice
    [ try $ parens parseScalarValue
    , VBool True  <$ reserved "true"
    , VBool False <$ reserved "false"
    , VBit  False <$ reserved "'0"
    , VBit  True  <$ reserved "'1"
    , VUnit       <$ parens whiteSpace
    , try $ VDouble Full <$> float
    , try $ VString      <$> stringLiteral
    , VInt <$> integer
    ]

{-------------------------------------------------------------------------------
  Variables with optional suffix
-------------------------------------------------------------------------------}

-- | Variable with optional suffix
--
-- >   IDENT "{" (IDENT "=" <expr>)*";")     -- struct init
-- > | IDENT ("." IDENT | "[" <range> "]")*  -- struct or array index
-- > | IDENT "(" <expr>*"," ")"              -- function call or cast
-- > | IDENT                                 -- variable
parseWithVarOnHead :: BlinkParser SrcExp
parseWithVarOnHead = choice
    [ try        $ withPos eStruct'     <*> identifier <*> braces (parseInit `sepBy1` requiredSemi)
    , try        $ withPos mkDeref      <*> identifier <*> many1 parseDerefSuffix
    , try $ join $ withPos mkCallOrCast <*> identifier <*> parens (parseExpr `sepBy` comma)
    , withPos mkVar <*> identifier
    ]
  where
    eStruct' loc () "complex" = eStruct loc () complex32TyName
    eStruct' loc () tn        = eStruct loc () tn

    mkDeref :: Maybe SourcePos -> () -> String -> [SrcExp -> SrcExp] -> SrcExp
    mkDeref p () x fs = foldr ($) (mkVar p () x) (reverse fs)

-- | Part of a struct definition
--
-- > IDENT "=" <expr>
parseInit :: BlinkParser (String, SrcExp)
parseInit = (,) <$> identifier <* symbol "=" <*> parseExpr

-- | Variable index (struct or array field)
--
-- > "." IDENT | "[" <range> "]"
parseDerefSuffix :: BlinkParser (SrcExp -> SrcExp)
parseDerefSuffix = choice
    [ withPos eProj'    <* symbol "." <*> identifier
    , withPos eArrRead' <*> brackets rangeParser
    ]
  where
    eProj'    loc a s e      = eProj    loc a e s
    eArrRead' loc a (y, l) x = eArrRead loc a x y l

mkVar :: Maybe SourcePos -> () -> String -> SrcExp
mkVar p () x = eVar p () (mkNameFromPos (Just x) (fromJust p) Nothing)

{-------------------------------------------------------------------------------
  Declarations and types
-------------------------------------------------------------------------------}

-- | Declarations
--
-- <decl> ::= "var" IDENT ":" <base-type> (":=" <expr>)?
declParser :: BlinkParser (Name, Ty, Maybe SrcExp)
declParser =
    withPos mkDecl <* reserved "var" <*> identifier
                   <* colon <*> parseBaseType
                   <*> optionMaybe (symbol ":=" *> parseExpr)
  where
    mkDecl p () x ty mbinit = (mkNameFromPos (Just x) (fromJust p) (Just ty), ty, mbinit)

-- | Base types
--
-- > <base-type> ::=
-- >     "()"
-- >   | "bit"
-- >   | "int"
-- >   | "int8"
-- >   | "int16"
-- >   | "int32"
-- >   | "int64"
-- >   | "double"
-- >   | "bool"
-- >   | "complex"
-- >   | "complex8"
-- >   | "complex16"
-- >   | "complex32"
-- >   | "complex64"
-- >   | "struct" IDENT
-- >   | "arr" "[" "length" IDENT "]" <base-type>
-- >   | "arr" "[" <expr> "]" <base-type>
-- >   | "arr" <base-type> -- length inferred from context
-- >   | "(" <base-type> ")"
--
-- where we must be able to evaluate the the <expr> for an array length to
-- a constant integer at compile time.
parseBaseType :: BlinkParser Ty
parseBaseType = choice
    [ TUnit                   <$ parens whiteSpace
    , TBit                    <$ reserved "bit"

    , tint                    <$ reserved "int"
    , tint8                   <$ reserved "int8"
    , tint16                  <$ reserved "int16"
    , tint32                  <$ reserved "int32"
    , tint64                  <$ reserved "int64"

    , TDouble Full            <$ reserved "double"
    , TBool                   <$ reserved "bool"

    , TStruct complex32TyName <$ reserved "complex"
    , TStruct complex8TyName  <$ reserved "complex8"
    , TStruct complex16TyName <$ reserved "complex16"
    , TStruct complex32TyName <$ reserved "complex32"
    , TStruct complex64TyName <$ reserved "complex64"

    , TStruct <$ reserved "struct" <*> identifier <?> "struct name"
    , reserved "arr" *> arrLength

    , parens parseBaseType
    ] <?> "expression type"
  where
    arrLength :: BlinkParser Ty
    arrLength = choice
      [ withPos mkFixed    <*> brackets intOrLength <*> parseBaseType
      , withPos mkInferred <*> parseBaseType
      ] <?> "array range and its base type"

    intOrLength :: BlinkParser (Either Int String)
    intOrLength = choice
      [ Right <$ reserved "length" <*> parens identifier
      , Left  <$> foldIntExpr
      ] <?> "array length description"

    mkFixed _ () (Left n)  t = let i = fromIntegral n
                               in TArr (Literal i) t
    mkFixed p () (Right x) t = let nm = mkNameFromPos (Just x) (fromJust p) Nothing
                               in TArr (NArr nm) t

    mkInferred p () t = let nm = mkNameFromPos Nothing (fromJust p) (Just tint)
                        in TArr (NVar nm 0) t

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Statement block
--
-- > <stmt-block> ::= "{" <stmts> "}" | <stmt>
--
-- TODO: If we disallow implicit final return then we need to document this.
parseStmtBlock :: BlinkParser SrcExp
parseStmtBlock = choice
  [ braces parseStmts
  , stmtToExp =<< parseStmt
  ] <?> "statement block"

-- | A list of commands
--
-- > <stmts> ::= <stmt>*";"
--
-- This follows the structure of `parseCommands` fairly closely, except that
-- we not record any bindings in the expression parser.
parseStmts :: BlinkParser SrcExp
parseStmts =
    foldStatements =<< parseStmt `sepEndBy1` optionalSemi
  where
    foldStatements :: [Statement] -> BlinkParser SrcExp
    foldStatements []  = error "This cannot happen"
    foldStatements [s] = stmtToExp s
    foldStatements (Right s:ss) = eSeq' s <$> foldStatements ss
    foldStatements (Left  k:ss) = k       <$> foldStatements ss

    eSeq' s1 s2 = eSeq (expLoc s2) () s1 s2

stmtToExp :: Statement -> BlinkParser SrcExp
stmtToExp (Left  _) = fail "Last statement in a block must be an expression"
stmtToExp (Right s) = return s

type Statement = Either (SrcExp -> SrcExp) SrcExp

-- | Statements
--
-- > <stmt> ::=
-- >     "let" <var-bind> "=" <expr> ("in" <stmt>)?
-- >   | <decl> ("in" <stmt>)?
-- >   <simple-stmt>
parseStmt :: BlinkParser Statement
parseStmt = choice
    [ join $ withPos eLet' <* reserved "let" <*> parseVarBind
                           <* symbol "=" <*> parseExpr
                           <*> optionMaybe (reserved "in" *> parseStmt)
    , join $ withPos eLetRef' <*> declParser
                              <*> optionMaybe (reserved "in" *> parseStmt)

    , Right <$> parseSimpleStmt
    ] <?> "statement"
  where
    eLet' p () x e (Just s) = do
      s' <- stmtToExp s
      return . Right $ eLet p () x AutoInline e s'
    eLet' p () x e Nothing =
      return . Left $ \m -> eLet p () x AutoInline e m

    eLetRef' p () (x, ty, e) (Just s) = do
      s' <- stmtToExp s
      return . Right $ eLetRef p () x ty e s'
    eLetRef' p () (x, ty, e) Nothing = do
      return . Left $ \m -> eLetRef p () x ty e m

-- | "Simple" statements (that do not expect a continuation)
--
-- > <simple-stmt> ::=
-- >   ("unroll" | "nounroll")? "for" <var-bind> "in" "[" <interval> "]" <stmt-block>
-- > | "while" "(" <expr> ")" <stmt-block>
-- > | "if" <expr> "then" <stmt-block> ("else" <stmt-block>)?
-- > | "return" <expr>
-- > | "print" <expr>*","
-- > | "println" <expr>*","
-- > | "error" STRING
-- > | IDENT "(" <expr>*"," ")"
-- > | IDENT ("." IDENT)* ("[" <range> "]")? ":=" <expr>
parseSimpleStmt :: BlinkParser SrcExp
parseSimpleStmt = choice
    [ withPos eFor'  <*> parseFor (reserved "for") <*> parseVarBind
                     <* reserved "in" <*> brackets genIntervalParser
                     <*> (parseStmtBlock <?> "for loop body")
    , withPos eWhile <* reserved "while" <*> parens parseExpr
                     <*> (parseStmtBlock <?> "while loop body")
    , withPos eIf    <* reserved "if"   <*> parseExpr
                     <* reserved "then" <*> (parseStmtBlock <?> "if branch")
                     <*> parseOptElse

    ,                              reserved "return"   *> parseExpr
    , withPos (makePrint False) <* reserved "print"   <*> parseExpr `sepBy` comma
    , withPos (makePrint True)  <* reserved "println" <*> parseExpr `sepBy` comma
    , withPos eError            <* reserved "error"   <*> stringLiteral

    , try $ withPos mkCall <*> identifier <*> parens (parseExpr `sepBy` comma)
    , withPos mkAssign <*> identifier
                       <*> many (withPos eProj' <* symbol "." <*> identifier)
                       <*> optionMaybe (brackets rangeParser)
                       <*  symbol ":="
                       <*> parseExpr
    ] <?> "statement"
  where
    parseOptElse :: BlinkParser SrcExp
    parseOptElse = choice
      [ withPos eunit <* notFollowedBy (reserved "else")
      , reserved "else" *> (parseStmtBlock <?> "else branch")
      ]

    eFor'  p () ui k (estart, elen) = eFor  p () ui k estart elen
    eProj' p () s e                 = eProj p () e s

    -- Print a series of expression; @b@ argument indicates whether we a newline
    makePrint b p () (h:t) = eSeq p () (ePrint p () False h) (makePrint b p () t)
    makePrint b p () []    = ePrint p () b (eVal p () (VString ""))

    mkAssign p () x ds Nothing rhs =
      eAssign p () (foldr ($) (mkVar p () x) ds) rhs
    mkAssign p () x ds (Just (estart, len)) rhs =
      eArrWrite p () (foldr ($) (mkVar p () x) ds) estart len rhs

{-------------------------------------------------------------------------------
  Small parsers
-------------------------------------------------------------------------------}

foldIntExpr :: BlinkParser Int
foldIntExpr = do
  e <- parseExpr <?> "expression"
  case evalInt e of
    Just i  -> return $ fromIntegral i
    Nothing -> parserFail "Non-constant array length expression."


-- | Variable with optional type annotation
--
-- > <var-bind> ::= IDENT | "(" IDENT ":" <base-type> ")"
parseVarBind :: BlinkParser Name
parseVarBind = choice
    [ withPos mkName <*> identifier
    , parens $ withPos mkNameTy <*> identifier <* symbol ":" <*> parseBaseType
    ] <?> "variable binding"
  where
    mkName   p () i    = mkNameFromPos (Just i) (fromJust p) Nothing
    mkNameTy p () i ty = mkNameFromPos (Just i) (fromJust p) (Just ty)

-- | Range
--
-- > <range> ::= <interval> | <expr>
rangeParser :: BlinkParser (SrcExp, LengthInfo)
rangeParser = choice
    [ try intervalParser
    , (\e -> (e, LISingleton)) <$> parseExpr
    ] <?> "range"

-- | Generalized interval parser
--
-- Returns @(start, length)@
--
-- In the grammar we don't distinguish between foldable expressions and
-- non-foldable expressions, so there this is equivalent to @<interval>@
genIntervalParser :: BlinkParser (SrcExp, SrcExp)
genIntervalParser = choice
    [ try $ withPos mkStartTo <*> foldIntExpr <* colon <*> foldIntExpr
    , mkStartLen <$> parseExpr <* comma <*> parseExpr
    ]
  where
    mkStartTo p () from to =
      let len = to - from + 1
      in (eVal p () (vint from), eVal p () (vint len))

    mkStartLen start len = (start, len)

-- | Interval
--
-- > <interval> ::= <expr> ":" <expr> | <expr> "," <expr>
intervalParser :: BlinkParser (SrcExp, LengthInfo)
intervalParser = choice
    [ try $ withPos mkStartTo <*> foldIntExpr <* colon <*> foldIntExpr
    , mkStartLen <$> parseExpr <* comma <*> foldIntExpr
    ]
  where
    mkStartTo p () from to =
       let len = to - from + 1
       in (eVal p () (vint from), LILength len)

    mkStartLen start len = (start, LILength len)

parseFor :: BlinkParser () -> BlinkParser UnrollInfo
parseFor for_reserved = choice
    [ for_reserved >> return AutoUnroll
    , reserved "unroll"   >> for_reserved >> return Unroll
    , reserved "nounroll" >> for_reserved >> return NoUnroll
    ]

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

mkNameFromPos :: Maybe String  -- ^ optional source name
              -> SourcePos     -- ^ source position
              -> Maybe Ty      -- ^ optional source type annotation
              -> Name
mkNameFromPos mb_src_nm spos mb_ty =
    toName (fromMaybe uniq mb_src_nm) (Just spos) mb_ty
  where
    uniq = "_t" ++ ln ++ "_" ++ col
    ln   = show (sourceLine spos)
    col  = show (sourceColumn spos)

mkCall :: Maybe SourcePos -> () -> String -> [SrcExp] -> SrcExp
mkCall p () fn args = eCall p () f args
  where
    f = eVar p () (mkNameFromPos (Just fn) (fromJust p) Nothing)

eunit :: Maybe SourcePos -> () -> SrcExp
eunit p () = eVal p () VUnit

-- | Create either a cast or a call since they share the same source syntax
mkCallOrCast :: Monad m => Maybe SourcePos -> () -> String -> [SrcExp] -> m SrcExp
mkCallOrCast p () x args
  | x == "int"       = assertSingleton args (cast tint)
  | x == "bit"       = assertSingleton args (cast TBit)
  | x == "double"    = assertSingleton args (cast tdouble)

  | x == "int64"     = assertSingleton args (cast tint64)
  | x == "int32"     = assertSingleton args (cast tint32)
  | x == "int16"     = assertSingleton args (cast tint16)
  | x == "int8"      = assertSingleton args (cast tint8)

  | x == "complex"   = assertSingleton args (cast tcomplex)
  | x == "complex8"  = assertSingleton args (cast tcomplex8)
  | x == "complex16" = assertSingleton args (cast tcomplex16)
  | x == "complex32" = assertSingleton args (cast tcomplex32)
  | x == "complex64" = assertSingleton args (cast tcomplex64)

  | otherwise        = return $ mkCall p () x args
  where
    cast t = eUnOp p () (Cast t)

assertSingleton :: Monad m => [t] -> (t -> a) -> m a
assertSingleton [e] action = return (action e)
assertSingleton _x _action = fail "Expecting only one argument!"
