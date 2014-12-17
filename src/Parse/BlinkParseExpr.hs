{- Copyright (c) Microsoft Corporation
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
{-# OPTIONS_GHC -Wall #-}
module BlinkParseExpr (
    -- * Top-level parsers
    parseExpr
  , declParser
  , parseBaseType
  , parseStmtBlock
  , parseStmts
  , paramsParser
    -- * Utilities
  , parseFor
  , parseVarBind
  , genIntervalParser
  , eunit
  ) where

import Control.Applicative ((<*>), (<$), (<*), (*>), (<$>))
import Control.Monad (join)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.Expr

import AstExpr
import AstUnlabelled
import BlinkLexer
import BlinkParseM
import Eval (evalInt)

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
-- >   | <elet-decl> "in" <expr>
-- >   | "if" <expr> "then" <expr> "else" <expr>
parseTerm :: BlinkParser SrcExp
parseTerm = choice
    [ parseUnit mkUnit
    , parens parseExpr
    , eLetDecl <$> parseELetDecl <* reserved "in" <*> parseExpr
    , withPos eIf <* reserved "if"   <*> parseExpr
                  <* reserved "then" <*> parseExpr
                  <* reserved "else" <*> parseExpr
    , parseValue
    , parseWithVarOnHead
    ] <?> "expression"
  where
    mkUnit p = eValSrc p VUnit

-- Parse "()"
--
-- NOTE: We wrap this in a 'try' always to avoid confusion with bracketed
-- expression/types.
parseUnit :: (Maybe SourcePos -> a) -> BlinkParser a
parseUnit f = try $ withPos f <* (symbol "(" >> symbol ")")

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Values
--
-- > <value> ::= <scalar-value> | "{" <scalar-value>*"," "}"
parseValue :: BlinkParser SrcExp
parseValue = choice
    [ withPos eValSrc    <*> parseScalarValue
    , withPos eValArrSrc <*> braces (sepBy parseScalarValue comma)
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
parseScalarValue :: BlinkParser Val
parseScalarValue = choice
    [ try $ parens parseScalarValue
    , VBool True  <$ reserved "true"
    , VBool False <$ reserved "false"
    , VBit  False <$ reserved "'0"
    , VBit  True  <$ reserved "'1"
    , parseUnit mkUnit
    , try $ VDouble <$> float
    , try $ VString <$> stringLiteral
    , VInt <$> integer
    ]
  where
    mkUnit _ = VUnit

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
    [ try        $ withPos eStruct'     <*> identifier <*> braces (parseInit `sepBy1` semi)
    , try        $ withPos mkDeref      <*> identifier <*> many1 parseDerefSuffix
    , try $ join $ withPos mkCallOrCast <*> identifier <*> parens (parseExpr `sepBy` comma)
    , withPos mkVar <*> identifier
    ]
  where
    eStruct' loc "complex" = eStruct loc (SrcTStruct complex32TyName)
    eStruct' loc tn        = eStruct loc (SrcTStruct tn)

    mkDeref :: Maybe SourcePos -> String -> [SrcExp -> SrcExp] -> SrcExp
    mkDeref p x fs = foldr ($) (mkVar p x) (reverse fs)

-- | Part of a struct definition
--
-- > IDENT "=" <expr>
parseInit :: BlinkParser (String, SrcExp)
parseInit = (,) <$> identifier <* reservedOp "=" <*> parseExpr

-- | Variable index (struct or array field)
--
-- > "." IDENT | "[" <range> "]"
parseDerefSuffix :: BlinkParser (SrcExp -> SrcExp)
parseDerefSuffix = choice
    [ withPos eProj'    <* reservedOp "." <*> identifier
    , withPos eArrRead' <*> brackets rangeParser
    ]
  where
    eProj'    loc s e      = eProj    loc e s
    eArrRead' loc (y, l) x = eArrRead loc x y l

mkVar :: Maybe SourcePos -> String -> SrcExp
mkVar p x = eVar p (toName x p SrcTyUnknown)

{-------------------------------------------------------------------------------
  Declarations and types
-------------------------------------------------------------------------------}

-- | Declarations
--
-- <decl> ::= "var" IDENT ":" <base-type> (":=" <expr>)?
declParser :: BlinkParser (GName SrcTy, Maybe SrcExp)
declParser =
    withPos mkDecl <* reserved "var" <*> identifier
                   <* colon <*> parseBaseType
                   <*> optionMaybe (reservedOp ":=" *> parseExpr)
  where
    mkDecl p x ty mbinit = (toName x p ty, mbinit)

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
parseBaseType :: BlinkParser SrcTy
parseBaseType = choice
    [ parseUnit mkUnit
    , SrcTBit          <$ identifier' "bit"

    , SrcTInt SrcBW32  <$ identifier' "int"
    , SrcTInt SrcBW8   <$ identifier' "int8"
    , SrcTInt SrcBW16  <$ identifier' "int16"
    , SrcTInt SrcBW32  <$ identifier' "int32"
    , SrcTInt SrcBW64  <$ identifier' "int64"

    , SrcTDouble       <$ identifier' "double"
    , SrcTBool         <$ identifier' "bool"

    , SrcTStruct complex32TyName <$ identifier' "complex"
    , SrcTStruct complex8TyName  <$ identifier' "complex8"
    , SrcTStruct complex16TyName <$ identifier' "complex16"
    , SrcTStruct complex32TyName <$ identifier' "complex32"
    , SrcTStruct complex64TyName <$ identifier' "complex64"

    , SrcTStruct <$ reserved "struct" <*> identifier <?> "struct name"
    , reserved "arr" *> arrLength

    , parens parseBaseType
    ] <?> "expression type"
  where
    arrLength :: BlinkParser SrcTy
    arrLength = choice
      [ withPos mkFixed    <*> brackets intOrLength <*> parseBaseType
      , withPos mkInferred <*> parseBaseType
      ] <?> "array range and its base type"

    intOrLength :: BlinkParser (Either Int String)
    intOrLength = choice
      [ Right <$ reserved "length" <*> parens identifier
      , Left  <$> foldIntExpr
      ] <?> "array length description"

    mkFixed _ (Left n)  t = let i = fromIntegral n
                            in SrcTArray (SrcLiteral i) t
    mkFixed p (Right x) t = let nm = toName x p SrcTyUnknown
                            in SrcTArray (SrcNArr nm) t

    mkInferred p t = SrcTArray (SrcNVar (fromJust p)) t

    mkUnit _ = SrcTUnit

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
  , parseStmtExp'
  ] <?> "statement block"

-- | A list of commands
--
-- > <stmts> ::= <stmt>*";"
--
-- This follows the structure of `parseCommands` fairly closely, except that
-- we not record any bindings in the expression parser.
parseStmts :: BlinkParser SrcExp
parseStmts = foldStatements =<< parseStmt `sepEndBy1` optional semi

-- | Statements
--
-- Comparable to `parseCommand`.
--
-- > <stmt> ::=
-- >     <elet-decl>
-- >   | <simple-stmt>
parseStmt :: BlinkParser Statement
parseStmt = choice
    [ try $ StmtDecl <$> parseELetDecl <* notFollowedBy (reserved "in")
    , StmtExp <$> parseStmtExp
    ] <?> "statement"

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
-- > | <elet-decl> "in" <expr>
-- > | IDENT "(" <expr>*"," ")"
-- > | IDENT ("." IDENT)* ("[" <range> "]")? ":=" <expr>
parseStmtExp :: BlinkParser SrcExp
parseStmtExp = choice
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
    , withPos eError'           <* reserved "error"   <*> stringLiteral

    , eLetDecl <$> parseELetDecl <* reserved "in" <*> parseStmtBlock

    , try $ withPos mkCall <*> identifier <*> parens (parseExpr `sepBy` comma)
    , withPos mkAssign <*> identifier
                       <*> many (withPos eProj' <* reservedOp "." <*> identifier)
                       <*> optionMaybe (brackets rangeParser)
                       <*  reservedOp ":="
                       <*> parseExpr
    ] <?> "statement"
  where
    parseOptElse :: BlinkParser SrcExp
    parseOptElse = choice
      [ withPos eunit <* notFollowedBy (reserved "else")
      , reserved "else" *> (parseStmtBlock <?> "else branch")
      ]

    eFor'   p ui k (estart, elen) = eFor   p ui k estart elen
    eProj'  p s e                 = eProj  p e s
    eError' p                     = eError p SrcTyUnknown

    -- Print a series of expression; @b@ argument indicates whether we a newline
    makePrint b p (h:t) = eSeq p (ePrint p False h) (makePrint b p t)
    makePrint b p []    = ePrint p b (eValSrc p (VString ""))

    mkAssign p x ds Nothing rhs =
      eAssign p (foldr ($) (mkVar p x) ds) rhs
    mkAssign p x ds (Just (estart, len)) rhs =
      eArrWrite p (foldr ($) (mkVar p x) ds) estart len rhs

-- We could define parseStmtExp' = parseStmtExp, but we use the below definition
-- to get better error messages. For example, both
--
-- > let x = 5
--
-- (as the last statement in a block) and
--
-- > let x = 5 in let y = 5;
--
-- (whether or not it is the last statement in a block) will now yield the
-- same error message.
parseStmtExp' :: BlinkParser SrcExp
parseStmtExp' = stmtToExp =<< parseStmt

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

-- | Statements
--
-- This is the equivalent to `Command` in the comp parser.
data Statement = StmtDecl ELetDecl | StmtExp SrcExp

-- | Local declarations
--
-- This is the equivalent of `LetDecl` in the comp parser.
data GELetDecl t a =
    ELetDeclERef    (Maybe SourcePos) (GName t, Maybe (GExp t a))
  | ELetDeclFunExpr (Maybe SourcePos) (GName t) [GName t] (GExp t a)
  | ELetDeclExpr    (Maybe SourcePos) (GName t) (GExp t a)

type ELetDecl = GELetDecl SrcTy ()

foldStatements :: [Statement] -> BlinkParser SrcExp
foldStatements = go
  where
    go []  = error "This cannot happen"
    go [s] = stmtToExp s
    go (StmtExp  e:ss) = eSeq' e    <$> go ss
    go (StmtDecl d:ss) = eLetDecl d <$> go ss

    eSeq' s1 s2 = eSeq (expLoc s2) s1 s2

stmtToExp :: Statement -> BlinkParser SrcExp
stmtToExp (StmtExp  e) = return e
stmtToExp (StmtDecl d) = fail $ unlines [
      "A block must end on an expression, it cannot end on a declaration."
    , "The declaration for " ++ show (eletDeclName d) ++ " appears unused?"
    ]

-- | Smart constructor for GELetDecl
--
-- Comparable to `cLetDecl`.
eLetDecl :: ELetDecl -> SrcExp -> SrcExp
eLetDecl (ELetDeclERef    p (xn, e)) = eLetRef    p xn e
eLetDecl (ELetDeclExpr    p x e)     = eLet       p x AutoInline e
eLetDecl (ELetDeclFunExpr p x ps e)  = eLetHeader p (mkFunDefined p x ps e)

-- | Local declarations in the expression language
--
-- Comparable to `parseLetDecl`.
--
-- > <elet-decl> ::=
-- >   | <decl>
-- >   | "let" <var-bind> "=" <expr>
parseELetDecl :: BlinkParser ELetDecl
parseELetDecl = choice
    [ withPos ELetDeclERef    <*> declParser
    , withPos ELetDeclFunExpr <* reserved "fun" <*> parseVarBind <*> paramsParser <*> braces parseStmts
    , withPos ELetDeclExpr    <* reserved "let" <*> parseVarBind <* reservedOp "=" <*> parseExpr
    ]

-- | Parameters to a (non-comp) function
--
-- > <params> ::= "(" (IDENT ":" <base-type>)*"," ")"
paramsParser :: BlinkParser [GName SrcTy]
paramsParser = parens $ sepBy paramParser (symbol ",")
  where
    paramParser = withPos mkParam <*> identifier <* colon <*> parseBaseType
    mkParam p x ty = toName x p ty

eletDeclName :: ELetDecl -> String
eletDeclName (ELetDeclERef    _ (nm, _)) = show nm
eletDeclName (ELetDeclFunExpr _ nm _ _)  = show nm
eletDeclName (ELetDeclExpr    _ nm _)    = show nm

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
parseVarBind :: BlinkParser (GName SrcTy)
parseVarBind = choice
    [ withPos mkName <*> identifier
    , parens $ withPos mkNameTy <*>
               identifier <* reservedOp ":" <*> parseBaseType
    ] <?> "variable binding"
  where
    mkName   p i    = toName i p SrcTyUnknown
    mkNameTy p i ty = toName i p ty

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
    mkStartTo p from to =
      let len = to - from + 1
      in (eValSrc p (vint from), eValSrc p (vint len))

    mkStartLen start len = (start, len)

-- | Interval
--
-- > <interval> ::= <expr> ":" <expr> | <expr> "," <expr>
intervalParser :: BlinkParser (SrcExp, LengthInfo)
intervalParser = choice
    [ try $ withPos mkStartTo <*> foldIntExpr <* colon <*> foldIntExpr
    , mkStartLen <$> parseExpr <* comma <*> orMetaVar foldIntExpr
    ]
  where
    mkStartTo p from to =
       let len = to - from + 1
       in (eValSrc p (vint from), LILength len)

    mkStartLen start (Left len) = (start, LILength len)
    mkStartLen start (Right x)  = (start, LIMeta x)

parseFor :: BlinkParser () -> BlinkParser UnrollInfo
parseFor for_reserved = choice
    [ for_reserved >> return AutoUnroll
    , reserved "unroll"   >> for_reserved >> return Unroll
    , reserved "nounroll" >> for_reserved >> return NoUnroll
    ]

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

orMetaVar :: BlinkParser a -> BlinkParser (Either a String)
orMetaVar p = (Left <$> try p) <|> (Right <$> identifier)

mkCall :: Maybe SourcePos -> String -> [SrcExp] -> SrcExp
mkCall p fn args = eCall p (toName fn p SrcTyUnknown) args

eunit :: Maybe SourcePos ->  SrcExp
eunit p = eValSrc p VUnit

-- | Create either a cast or a call since they share the same source syntax
mkCallOrCast :: Monad m => Maybe SourcePos -> String -> [SrcExp] -> m SrcExp
mkCallOrCast p x args
  | x == "int"       = assertSingleton args (cast tintSrc)
  | x == "bit"       = assertSingleton args (cast SrcTBit)
  | x == "double"    = assertSingleton args (cast tdoubleSrc)

  | x == "int64"     = assertSingleton args (cast tintSrc64)
  | x == "int32"     = assertSingleton args (cast tintSrc32)
  | x == "int16"     = assertSingleton args (cast tintSrc16)
  | x == "int8"      = assertSingleton args (cast tintSrc8)

  | x == "complex"   = assertSingleton args (cast tcomplexSrc)
  | x == "complex8"  = assertSingleton args (cast tcomplexSrc8)
  | x == "complex16" = assertSingleton args (cast tcomplexSrc16)
  | x == "complex32" = assertSingleton args (cast tcomplexSrc32)
  | x == "complex64" = assertSingleton args (cast tcomplexSrc64)

  | otherwise        = return $ mkCall p x args
  where
    cast t = eUnOp p (Cast t)

assertSingleton :: Monad m => [t] -> (t -> a) -> m a
assertSingleton [e] action = return (action e)
assertSingleton _x _action = fail "Expecting only one argument!"

eValSrc :: Maybe SourcePos -> Val -> SrcExp
eValSrc p v = eVal p SrcTyUnknown v

eValArrSrc :: Maybe SourcePos -> [Val] -> SrcExp
eValArrSrc p vs = eValArr p SrcTyUnknown vs
