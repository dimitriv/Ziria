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
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module BlinkParseComp ( parseProgram, runParseM ) where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>))
import Control.Arrow (second)
import Control.Monad.Reader.Class
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.Expr

import AstComp
import AstExpr
import BlinkParseExpr

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | > <program> ::= <decls> <comp>
parseProgram :: BlinkParser (Prog () ())
parseProgram = MkProg <$ whiteSpace <*> declsParser <*> parseComp

-- | > <decls> ::= (<decl>;)*
--
-- (declParser comes from the expression language)
declsParser :: BlinkParser [(Name, Ty, Maybe SrcExp)]
declsParser = declParser `endBy` semi

-- | Parse a computation expression
--
-- > <comp> ::=
-- >     "standalone" <comp>
-- >   | "repeat" <vect-ann>? <comp>
-- >   | "until" <expr> <comp>
-- >   | "while" <expr> <comp>
-- >   | "times" <expr> <comp>
-- >   | "for" <var-bind> "in" "[" <interval> "]" <comp>
-- >
-- >   | <comp> ">>>" <comp>
-- >   | <comp> "|>>>|" <comp>
-- >
-- >   | <term>
--
-- where
--
-- * the prefix operators `standalone`, `repeat`, `until`, `while`, `times` and
--   `for` all have the same precedence, and all bind stronger than the infix
--   operators `>>>` and `|>>>|`
-- * `>>>` and `|>>>|` are both left associative
-- * `>>>` binds stronger than `|>>>|`
--
-- TODOs:
-- * Is the relative precedence of `>>>` and `|>>>|` intended?
-- * We don't have any tests at all for 'standalone'. Is it obsolete?
parseComp :: BlinkParser SrcComp
parseComp =
    buildExpressionParser table parseCompTerm
  where
    table =
      [ [ xPrefix $ withPos cStandalone <*  reserved "standalone"
        , xPrefix $ withPos cRepeat     <*  reserved "repeat" <*> optVectAnn
        , xPrefix $ withPos cUntil      <*  reserved "until"  <*> parseExpr
        , xPrefix $ withPos cWhile      <*  reserved "while"  <*> parseExpr
        , xPrefix $ withPos cTimes'     <*> timesPref
        , xPrefix $ withPos cTimes'     <*> forPref
        ]
      , [ lInfix $ withPos cPar <*> opMaybePipeline  ]
      , [ lInfix $ withPos cPar <*> opAlwaysPipeline ]
      ]

    cTimes' = uncurry4 .: cTimes

-- | A term in a computation expression
--
-- > term ::=
-- >     "(" <comp> ")"
-- >   | "return" <expr>"
-- >   | "emit" <expr>
-- >   | "emits" <expr>
-- >   | "takes" <expr>
-- >   | "filter" <expr>
-- >   | "read" <type-ann>?
-- >   | "write" <type-ann>?
-- >   | "map" <vect-ann>? <var-bind>
-- >   | "take"
-- >   | <var-or-call>
-- >   | "if" <expr> "then" <comp> "else" <comp>
-- >   | <let-decl> "in" <comp>
-- >   | "do" <stmt-block>
-- >   | "seq"? "{" <commands> "}"
--
-- `<stmt_block>` comes from the expression language.
parseCompTerm :: BlinkParser SrcComp
parseCompTerm = choice
    [ parens parseComp

    , withPos cReturn'  <* reserved "return" <*> parseExpr
    , withPos cEmit     <* reserved "emit"   <*> parseExpr
    , withPos cEmits    <* reserved "emits"  <*> parseExpr
    , withPos cTake     <* reserved "takes"  <*> parseExpr
    , withPos cFilter   <* reserved "filter" <*> parseExpr
    , withPos cReadSrc  <* reserved "read"   <*> optTypeAnn
    , withPos cWriteSnk <* reserved "write"  <*> optTypeAnn
    , withPos cMap      <* reserved "map"    <*> optVectAnn <*> parseVarBind
    , withPos cTake1    <* reserved "take"

    , parseVarOrCall

    , withPos cBranch <* reserved "if"   <*> parseExpr
                      <* reserved "then" <*> parseComp
                      <* reserved "else" <*> parseComp
    , withPos cLetDecl <*> parseLetDecl `bindExtend` \f -> f <$ reserved "in" <*> parseComp
    , withPos cReturn' <* reservedOp "do" <*> parseStmtBlock
    , optional (reserved "seq") >> braces parseCommands
    ] <?> "computation"
  where
    cReturn' = ($ AutoInline) .: cReturn

{-------------------------------------------------------------------------------
  Operators (used to build parseComp)

  These are not assigned non-terminals of their own in the grammar.
-------------------------------------------------------------------------------}

-- > ">>>"
opMaybePipeline :: BlinkParser ParInfo
opMaybePipeline = mkParInfo MaybePipeline <$ reservedOp ">>>"

-- > "|>>>|"
opAlwaysPipeline :: BlinkParser ParInfo
opAlwaysPipeline = mkParInfo (AlwaysPipeline 0 0) <$ reservedOp "|>>>|"

-- > "times" <expr>
timesPref :: BlinkParser (UnrollInfo, SrcExp, SrcExp, Name)
timesPref =
    (withPos mkTimes <*> parseFor (reserved "times") <*> parseExpr) <?> "times"
  where
    mkTimes p () ui e =
      let nm = mkNameFromPos (Just "_tmp_count") (fromJust p) (Just tint)
      in (ui, eVal p () (VInt 0), e, nm)

-- "for" <var-bind> "in" "[" <interval> "]"
forPref :: BlinkParser (UnrollInfo, SrcExp, SrcExp, Name)
forPref =
    (withPos mkFor <*> parseFor (reserved "for") <*> parseVarBind
                   <* reserved "in" <*> brackets genIntervalParser) <?> "for"
  where
    mkFor _p () ui k (estart, elen) = (ui, estart,elen,k)

{-------------------------------------------------------------------------------
  Variable or call
-------------------------------------------------------------------------------}

-- | Variable or function call
parseVarOrCall :: BlinkParser SrcComp
parseVarOrCall = go <?> "variable or function call"
  where
    go = do
      p <- getPosition
      x <- identifier
      let xnm = mkNameFromPos (Just x) p Nothing
      choice [ do notFollowedBy (symbol "(")
                  notFollowedBy (symbol "<-")
                  return (cVar (Just p) () xnm)
             , withPos (($ xnm) .: cCall) <*> parseArgs xnm
             ] <?> "variable or function call"

-- | Parse an argument list
--
-- If `xnm` is not a known defined function, this is simply
--
-- > <arglist> ::= "(" <expr>*"," ")"
--
-- If however `xnm` is a known function we expect a comma-separated list of
-- as many arguments as the function expect, which can either be computations
-- `<comp>` or expressions `<expr>`.
parseArgs :: Name -> BlinkParser [CallArg SrcExp SrcComp]
parseArgs xnm = parens $ do
    penv <- ask
    case lookup xnm penv of
      Just arg_info -> map parseArg arg_info `sepsBy` comma
      Nothing       -> (CAExp <$> parseExpr) `sepBy`  comma
  where
    parseArg :: (Name, CallArg Ty CTy0) -> BlinkParser (CallArg SrcExp SrcComp)
    parseArg (_, CAExp  _) = CAExp  <$> parseExpr
    parseArg (_, CAComp _) = CAComp <$> parseComp

{-------------------------------------------------------------------------------
  Let statements
-------------------------------------------------------------------------------}

data LetDecl =
    LetDeclVar (Name, Ty, Maybe SrcExp)
  | LetDeclStruct StructDef
  | LetDeclExternal String [(Name, Ty)] Ty
  | LetDeclFunComp (Maybe (Int, Int)) Name [(Name, CallArg Ty CTy0)] [(Name, Ty, Maybe SrcExp)] SrcComp
  | LetDeclComp (Maybe (Int, Int)) Name SrcComp
  | LetDeclFunExpr Name [(Name, Ty)] [(Name, Ty, Maybe SrcExp)] SrcExp
  | LetDeclExpr Name SrcExp

-- | The thing that is being declared in a let-statemnt
--
-- > <let-decl> ::=
-- >   | <decl>
-- >   | <struct>
-- >   | "let" "external" IDENT <params> ":" <base-type>
-- >   | "let" <comp-ann> <var-bind> <comp-params> "=" <decl>* <commands>
-- >   | "let" <comp-ann> <var-bind> "=" <commands>
-- >   | "let" <var-bind> <params> "=" <decl>* <stmts>
-- >   | "let" <var-bind> "=" <expr>
parseLetDecl :: BlinkParser LetDecl
parseLetDecl = choice
    [ LetDeclVar <$> declParser
    , LetDeclStruct <$> parseStruct
    , try $ LetDeclExternal <$ reserved "let" <* reserved "external" <*> identifier <*> paramsParser <* symbol ":" <*> parseBaseType
    , try $ LetDeclFunComp  <$ reserved "let" <*> parseCompAnn <*> parseVarBind <*> compParamsParser <* symbol "=" <*> declsParser <*> nest parseCommands
    , try $ LetDeclComp     <$ reserved "let" <*> parseCompAnn <*> parseVarBind                      <* symbol "="                 <*> nest parseCommands
    , try $ LetDeclFunExpr  <$ reserved "let"                  <*> parseVarBind <*> paramsParser     <* symbol "=" <*> declsParser <*> nest parseStmts
    , try $ LetDeclExpr     <$ reserved "let"                  <*> parseVarBind                      <* symbol "="                 <*> nest parseExpr
    ]

-- > <struct> ::= "struct" IDENT "=" "{" (IDENT ":" <base-type>)*";" "}"
parseStruct :: BlinkParser StructDef
parseStruct = do
    reserved "struct"
    x <- identifier
    _ <- symbol "="
    braces $ StructDef x <$> parseField `sepBy` semi
  where
    parseField = (,) <$> identifier <* colon <*> parseBaseType

-- | Parameters to a (non-comp) function
--
-- > <params> ::= "(" (IDENT ":" <base-type>)*"," ")"
paramsParser :: BlinkParser [(Name, Ty)]
paramsParser = parens $ sepBy paramParser (symbol ",")
  where
    paramParser = withPos mkParam <*> identifier <* colon <*> parseBaseType
    mkParam p () x ty = (mkNameFromPos (Just x) (fromJust p) (Just ty), ty)

-- | Parameters to a (comp) function
--
-- > <comp-params> ::= "(" (IDENT ":" (<base-type> | <comp-base-type>))*"," ")"
--
-- (<base-type> comes from the expr parser; <comp-base-type> is defined here).
compParamsParser :: BlinkParser [(Name, CallArg Ty CTy0)]
compParamsParser = parens $ sepBy paramParser (symbol ",")
  where
    paramParser = withPos mkParam <*> identifier <* colon <*> parseType
    parseType   = choice [ CAExp  <$> parseBaseType
                         , CAComp <$> parseCompBaseType
                         ] <?> "computation parameter type"

    mkParam p () x mty = (mkNameFromPos (Just x) (fromJust p) Nothing, mty)

-- | Computation type
--
-- > <comp-base-type> ::= "ST" ("T" | "C" <base-type>) <base-type> <base-type>
parseCompBaseType :: BlinkParser CTy0
parseCompBaseType = choice
    [ mkCTy0 <$ reserved "ST" <*> parse_idx <*> parseBaseType <*> parseBaseType
    , parens parseCompBaseType
    ] <?> "computation type"
  where
    parse_idx = choice
      [ Nothing <$ reserved "T"
      , Just    <$ reserved "C" <*> parseBaseType
      , parens parse_idx
      ] <?> "computation type index"

    mkCTy0 Nothing   ti ty = TTrans ti ty
    mkCTy0 (Just tv) ti ty = TComp tv ti ty


cLetDecl :: Maybe SourcePos -> () -> LetDecl -> (ParseCompEnv, SrcComp -> SrcComp)
cLetDecl p () (LetDeclVar (xn, ty, mbinit)) =
    ([], cLetERef p () xn bnd)
  where
    bnd = case mbinit of Nothing -> Left ty
                         Just ei -> Right ei
cLetDecl p () (LetDeclStruct sdef) =
    ([], cLetStruct p () sdef)
cLetDecl p () (LetDeclExternal x params ty) =
    ([], cLetHeader p () fn fun)
  where
    fn  = mkNameFromPos (Just x) (fromJust p) Nothing
    fun = MkFun (MkFunExternal fn params ty) p ()
cLetDecl p () (LetDeclFunComp h x params locls c) =
    ([(x, params)], cLetFunC p () x params locls (mkVectComp c h))
cLetDecl p () (LetDeclComp h x c) =
    ([], cLet p () x (mkVectComp c h))
cLetDecl p () (LetDeclFunExpr x params locls e) =
    ([], cLetHeader p () x fun)
  where
    fun = MkFun (MkFunDefined x params locls e) p ()
cLetDecl p () (LetDeclExpr x e) =
    ([], cLetE p () x AutoInline e)

mkVectComp :: SrcComp -> Maybe (Int,Int) -> SrcComp
mkVectComp sc Nothing  = sc
mkVectComp sc (Just h) = cVectComp (compLoc sc) (compInfo sc) h sc

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

type Command = Either (SrcComp -> SrcComp) SrcComp

-- | A list of commands
--
-- Technically speaking, we cannot give a context free grammar here because the
-- parser for function calls depends on the functions that are in scope.
-- However, we ignore this in the documentation where we simply say
--
-- > <commands> ::= (<command> ";")*
parseCommands :: BlinkParser SrcComp
parseCommands =
    parseCommand `bindExtend` go . (:[])
  where
    go :: [Command] -> BlinkParser SrcComp
    go cs = do
      optional semi
      choice [ parseCommand `bindExtend` go . (:cs)
             , foldCommands (reverse cs)
             ]

    foldCommands :: [Command] -> BlinkParser SrcComp
    foldCommands []           = error "This cannot happen"
    foldCommands [Right c]    = return c
    -- TODO: We should really give an error for the @[Left k]@ case.
    -- ("last statement in a seq block should be a computation")
    foldCommands [Left k]     = k <$> withPos cunit
    foldCommands (Right c:cs) = cSeq' c <$> foldCommands cs
    foldCommands (Left k:cs)  = k       <$> foldCommands cs

    cSeq' c1 c2 = cSeq (compLoc c2) () c1 c2

-- | Commands
--
-- > <command> ::=
-- >     <let-decl>
-- >   | "if" <expr> "then" <comp>
-- >   | <var-bind> "<-" <comp>
-- >   | <comp>
parseCommand :: BlinkParser (ParseCompEnv, Command)
parseCommand = choice
    [ try $ withPos cLetDecl' <*> parseLetDecl <* notFollowedBy (reserved "in")
    , try $ withPos cBranch' <* reserved "if"   <*> parseExpr
                             <* reserved "then" <*> parseComp
                             <* notFollowedBy (reserved "else")
    , try $ withPos cBindMany' <*> parseVarBind <* symbol "<-" <*> parseComp
    , (\c -> ([], Right c)) <$> parseComp
    ] <?> "command"
  where
    cLetDecl' = second Left ..: cLetDecl
    cBranch'   loc a e c1 = ([], Right $ cBranch loc a e c1 (cunit loc ()))
    cBindMany' loc a x c  = ([], Left $ \c' -> cBindMany loc a c [(x, c')])

cunit :: Maybe SourcePos -> () -> SrcComp
cunit p a = cReturn p a ForceInline (eunit (fromJust p))

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

-- | Optional type annotation
--
-- > <type-ann> ::= "[" <base-type> "]"
optTypeAnn :: BlinkParser RWTypeAnn
optTypeAnn =
    mkTypeAnn <$> optionMaybe (brackets parseBaseType)
  where
    mkTypeAnn Nothing  = RWNoTyAnn
    mkTypeAnn (Just t) = RWRealTyAnn t

-- | Vectorization annotation
--
-- > <vect-ann> ::= "<="? "!"? <range>
parseVectAnn :: BlinkParser VectAnn
parseVectAnn =
    mkVectAnn <$> optionMaybe (reservedOp "<=") <*> parseVectAnnFlag
  where
    mkVectAnn Nothing   = uncurry Rigid
    mkVectAnn (Just ()) = uncurry UpTo

-- | Parses just the @[(i,j)]@ annotation
parseVectAnnFlag :: BlinkParser (Bool, (Int, Int))
parseVectAnnFlag =
    mkFlag <$> optionMaybe (reserved "!") <*> parseRange
  where
    mkFlag Nothing   v = (True,  v)
    mkFlag (Just ()) v = (False, v)

-- | Range
--
-- > <range> ::= "[" <int> "," <int> "]"
parseRange :: BlinkParser (Int, Int)
parseRange = brackets $ mkRange <$> integer <* comma <*> integer
  where
    mkRange i j = (fromInteger i, fromInteger j)

-- | Shorthand for @<vect-ann>?@
optVectAnn :: BlinkParser (Maybe VectAnn)
optVectAnn = optionMaybe parseVectAnn

-- > <comp-ann> ::= "comp" <range>?
parseCompAnn :: BlinkParser (Maybe (Int, Int))
parseCompAnn = reserved "comp" *> optionMaybe parseRange

{-------------------------------------------------------------------------------
  Auxiliary BlinkParser
-------------------------------------------------------------------------------}

withPos :: (Maybe SourcePos -> () -> a) -> BlinkParser a
withPos constr = do
  p <- getPosition
  return $ constr (Just p) ()

bindExtend :: BlinkParser (ParseCompEnv, a) -> (a -> BlinkParser b) -> BlinkParser b
bindExtend x f = x >>= \(env, a) -> extendParseEnv env $ f a

infixl 1 `bindExtend`  -- Same precedence as (>>=)

-- TODO: Should we use a bracket-like construct here?
nest :: BlinkParser a -> BlinkParser a
nest p = do
  updateState $ \x -> x + 1
  a <- p
  updateState $ \x -> x - 1
  return a

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

uncurry4 :: (a -> b -> c -> d -> z) -> (a, b, c, d) -> z
uncurry4 fun (a, b, c, d) = fun a b c d

(.:) :: (y -> z) -> (a -> b -> y) -> a -> b -> z
(.:) fun' fun a b = fun' (fun a b)

(..:) :: (y -> z) -> (a -> b -> c -> y) -> a -> b -> c -> z
(..:) fun' fun a b c = fun' (fun a b c)

-- | Like parsec's `Prefix` but allow repeated application of the operator.
--
-- See http://stackoverflow.com/a/10475767/742991.
xPrefix :: Stream s m t => ParsecT s u m (a -> a) -> Operator s u m a
xPrefix op = Prefix . chainl1 op $ return (.)

-- | Left-associative `Infix`
lInfix :: ParsecT s u m (a -> a -> a) -> Operator s u m a
lInfix op = Infix op AssocLeft

-- | Variant on `sepBy` that takes a list of parsers rather than repeating one
sepsBy :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m sep -> ParsecT s u m [a]
sepsBy []     _  = return []
sepsBy [p]    _  = (:[]) <$> p
sepsBy (p:ps) op = (:) <$> p <* op <*> sepsBy ps op
