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
             ScopedTypeVariables,
             FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module BlinkParseComp (parseProgram, parseCmdComp, runParseM) where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>))
import Control.Monad (join)
import Control.Monad.Reader.Class
import Text.Parsec
import Text.Parsec.Expr

import AstComp
import AstExpr
import AstUnlabelled
import BlinkParseExpr
import BlinkLexer
import BlinkParseM
import Interpreter (evalSrcInt)

import Utils ( uncurry4 )

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | > <program> ::= <decls> <let-decls>*
--
-- TODO: Update the grammar
parseProgram :: BlinkParser SrcProg
parseProgram = startOfFile <* many fileNameChange *> parseTopLevel <* eof

parseTopLevel :: BlinkParser SrcProg
parseTopLevel = MkProg <$>
    (foldCommands =<< extractMain [] =<< (parseLetDecl `extSepEndBy1` topLevelSep))
  where
    extractMain :: [LetDecl] -> [LetDecl] -> BlinkParser [Command]
    extractMain _   []     = fail "No main found"
    extractMain acc (d:ds) =
      case d of
        LetDeclComp _p Nothing nm c | name nm == "main" ->
          return $ map CmdDecl (reverse acc ++ ds) ++ [CmdComp c]
        _ ->
          extractMain (d:acc) ds

topLevelSep :: BlinkParser ()
topLevelSep = many fileNameChange *> optional semi <* many fileNameChange

-- | Parse a computation expression
--
-- > <comp> ::=
-- >     "standalone" <comp>
-- >   | "repeat" <vect-ann>? <comp>
-- >   | "until" <expr> <comp>
-- >   | "while" <expr> <comp>
-- >   | ("unroll" | "nounroll")? "times" <expr> <comp>
-- >   | ("unroll" | "nounroll")? "for" <var-bind> "in" "[" <interval> "]" <comp>
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
parseCmdComp :: BlinkParser SrcComp
parseCmdComp =
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

    cTimes' = uncurry4 . cTimes

-- | See discussion of `parseStmtExp'`
parseCmdComp' :: BlinkParser SrcComp
parseCmdComp' = cmdToComp =<< parseCommand

-- | A term in a computation expression
--
-- > term ::=
-- >     "(" <comp> ")"
-- >   | "return" <inl-ann>? <expr>"
-- >   | "emit" <expr>
-- >   | "emits" <expr>
-- >   | "takes" <expr>
-- >   | "filter" <expr>
-- >   | "read" <type-ann>?
-- >   | "write" <type-ann>?
-- >   | "map" <vect-ann>? <var-bind>
-- >   | "take"
-- >   | "if" <expr> "then" <comp> "else" <comp>
-- >   | <let-decl> "in" <comp>
-- >   | "do" <stmt-block>
-- >   | "seq"? "{" <commands> "}"
-- >   | <var-or-call>
--
-- `<stmt_block>` comes from the expression language.
parseCompTerm :: BlinkParser SrcComp
parseCompTerm = choice
    [ parens parseCmdComp

    , withPos cReturn' <*> optInlAnn <* reserved "return" <*> parseExpr
    , withPos cEmit'    <* reserved "emit"   <*> parseExpr
    , withPos cEmits'   <* reserved "emits"  <*> parseExpr
    , join $ withPos cTake' <* reserved "takes"  <*> parseExpr
    , withPos cFilter   <* reserved "filter" <*> parseVarBind

    , withPos cReadSrc  <* reserved "read"   <*> optTypeAnn
    , withPos cWriteSnk <* reserved "write"  <*> optTypeAnn
    , withPos cMap      <* reserved "map"    <*> optVectAnn <*> parseVarBind
    , withPos cTake1'   <* reserved "take"

    , withPos cBranch' <* reserved "if"   <*> parseExpr
                       <* reserved "then" <*> parseCmdComp
                       <*> optionMaybe (reserved "else" *> parseCmdComp)

    , parseLetDecl `extBind` \d -> cLetDecl d <$ reserved "in" <*> parseCmdComp'

    , withPos cReturn' <*> return AutoInline <* reserved "do" <*> parseStmtBlock
    , optional (reserved "seq") >> braces parseCommands

    , parseVarOrCall
    ] <?> "computation"
  where
    cReturn' p = cReturn p
    cEmit'   p = cEmit   p
    cEmits'  p = cEmits  p
    cTake1'  p = cTake1  p SrcTyUnknown

    cTake' p e = do
      case evalSrcInt e of
        (Right n,   _prints) -> return $ cTake p SrcTyUnknown (fromInteger n)
        (Left _err, _prints) -> fail "Non-constant argument to takes"

    cBranch' p cond true (Just false) = cBranch p cond true false
    cBranch' p cond true Nothing      = cBranch p cond true (cunit p)

{-------------------------------------------------------------------------------
  Operators (used to build parseCmdComp)

  These are not assigned non-terminals of their own in the grammar.
-------------------------------------------------------------------------------}

-- > ">>>"
opMaybePipeline :: BlinkParser ParInfo
opMaybePipeline = mkParInfo MaybePipeline <$ reservedOp ">>>"

-- > "|>>>|"
opAlwaysPipeline :: BlinkParser ParInfo
opAlwaysPipeline = mkParInfo (AlwaysPipeline 0 0) <$ reservedOp "|>>>|"

-- > ("unroll" | "nounroll")? "times" <expr>
timesPref :: BlinkParser (UnrollInfo, SrcExp, SrcExp, GName SrcTy)
timesPref = try
    ((withPos mkTimes <*> parseFor (reserved "times") <*> parseExpr) <?> "times")
  where
    -- NOTE: It doesn't matter that we only have 32-bit iterators here, since
    -- in a 'times' loop the code doesn't get access to the iterator anyway,
    -- so there is no possibility to cast the iterator to a different type.
    mkTimes p ui e =
      let nm = toName "_tmp_count" p tintSrc
      in (ui, eVal p tintSrc (VInt 0), e, nm)

-- > ("unroll" | "nounroll")? "for" <var-bind> "in" "[" <interval> "]"
forPref :: BlinkParser (UnrollInfo, SrcExp, SrcExp, GName SrcTy)
forPref = try
    ((withPos mkFor <*> parseFor (reserved "for") <*> parseVarBind
                    <* reserved "in" <*> brackets genIntervalParser) <?> "for")
  where
    mkFor _p ui k (estart, elen) = (ui, estart,elen,k)

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
      let xnm = toName x (Just p) SrcCTyUnknown
      choice [ do notFollowedBy (symbol "(")
                  notFollowedBy (reservedOp "<-")
                  return (cVar (Just p) xnm)
             , withPos (\pos -> cCall pos xnm) <*> parseArgs xnm
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
parseArgs :: GName SrcCTy -> BlinkParser [CallArg SrcExp SrcComp]
parseArgs xnm = parens $ do
    penv <- ask
    let spec = case lookup xnm penv of
                 Just argInfo -> map (parseArg . nameTyp) argInfo
                              ++ repeat (CAExp <$> parseExpr)
                 Nothing      -> repeat (CAExp <$> parseExpr)
    spec `sepsBy` comma
  where
    parseArg :: CallArg t tc -> BlinkParser (CallArg SrcExp SrcComp)
    parseArg (CAExp  _) = CAExp  <$> parseExpr
    parseArg (CAComp _) = CAComp <$> parseCmdComp

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | Commands
--
-- This is the equivalent to `Statement` in the expr parser.
data Command = CmdDecl LetDecl | CmdBind Bind | CmdComp SrcComp

-- | Binding site
data Bind = Bind (Maybe SourcePos) (GName SrcTy) SrcComp

-- | Declarations
--
-- This is the equivalent of `GELetDecl` and co in the expression parser.
data GLetDecl tc t a b =
    LetDeclERef     (Maybe SourcePos) (GName t, Maybe (GExp t b))
  | LetDeclStruct   (Maybe SourcePos) (GStructDef t)
  | LetDeclExternal (Maybe SourcePos) String [GName t] t
  | LetDeclFunComp  (Maybe SourcePos) (Maybe (Int, Int)) (GName tc) [GName (CallArg t tc)] (GComp tc t a b)
  | LetDeclFunExpr  (Maybe SourcePos) (GName t) [GName t] (GExp t b)
  | LetDeclComp     (Maybe SourcePos) (Maybe (Int, Int)) (GName tc) (GComp tc t a b)
  | LetDeclExpr     (Maybe SourcePos) (GName t) (GExp t b)

type LetDecl = GLetDecl SrcCTy SrcTy () ()

-- | Commands
--
-- Comparable to `parseStmt`.
--
-- > <command> ::=
-- >     <let-decl>
-- >   | <var-bind> "<-" <comp>
-- >   | <comp>
parseCommand :: BlinkParser Command
parseCommand = choice
    [ try $ CmdDecl <$> parseLetDecl <* notFollowedBy (reserved "in")
    , try $ CmdBind <$> parseBind
    , CmdComp <$> parseCmdComp
    ] <?> "command"

-- | A list of commands
--
-- Technically speaking, we cannot give a context free grammar here because the
-- parser for function calls depends on the functions that are in scope.
-- However, we ignore this in the documentation where we simply say
--
-- > <commands> ::= (<command> ";")*
--
-- The last <command> must be a computation.
parseCommands :: BlinkParser SrcComp
parseCommands = foldCommands =<< (parseCommand `extSepEndBy1` optional semi)

-- | > `x <- comp`
parseBind :: BlinkParser Bind
parseBind = withPos Bind <*> parseVarBind <* reservedOp "<-" <*> parseCmdComp

-- | Fold a list of commands into a single computation, attempting to give an
-- intelligible error if the last command is not a computation (#53, #56).
foldCommands :: [Command] -> BlinkParser SrcComp
foldCommands = go
  where
    go []             = error "This cannot happen"
    go [c]            = cmdToComp c
    go (CmdDecl d:cs) = cLetDecl d <$> go cs
    go (CmdBind b:cs) = cBind    b <$> go cs
    go (CmdComp c:cs) = cSeq'    c <$> go cs

    cSeq' c1 c2 = cSeq (compLoc c2) c1 c2
    cBind (Bind p x c1) c2 = cBindMany p c1 [(x, c2)]

cmdToComp :: Command -> BlinkParser SrcComp
cmdToComp = go
  where
    go (CmdDecl d) = declError d
    go (CmdBind b) = bindError b
    go (CmdComp c) = return c

    declError decl = fail $ unlines [
        "A block must end on a computation, it cannot end on a declaration."
      , "The declaration for " ++ show (letDeclName decl) ++ " appears unused?"
      ]

    bindError (Bind _ x _) = fail $ unlines [
        "A block must end on a computation, it cannot end on a bind."
      , "Try removing the '" ++ show x ++ " <-' part perhaps?"
      ]

-- | The thing that is being declared in a let-statemnt
--
-- Comparable to `parseELetDecl`.
--
-- > <let-decl> ::=
-- >   | <decl>
-- >   | <struct>
-- >   | "fun" "external" IDENT <params> ":" <base-type>
-- >   | "fun" <comp-ann> <cvar-bind> <comp-params> "{" <decl>* <commands> "}"
-- >   | "fun" <var-bind> <params> "{" <decl>* <stmts> "}"
-- >   | "let" <comp-ann> <cvar-bind> "=" <comp>
-- >   | "let" <var-bind> "=" <expr>
parseLetDecl :: BlinkParser LetDecl
parseLetDecl = choice
    [ withPos LetDeclERef     <*> declParser
    , withPos LetDeclStruct   <*> parseStruct
    , withPos LetDeclExternal <*  prefExt  <*> identifier    <*> paramsParser     <* reservedOp ":" <*> parseBaseType
    , withPos LetDeclFunComp  <*> prefFunC <*> parseCVarBind <*> compParamsParser <*> braces parseCommands
    , withPos LetDeclFunExpr  <*  prefFun  <*> parseVarBind  <*> paramsParser     <*> braces parseStmts
    , withPos LetDeclComp     <*> prefLetC <*> parseCVarBind <* reservedOp "=" <*> parseCmdComp
    , withPos LetDeclExpr     <*  prefLet  <*> parseVarBind  <* reservedOp "=" <*> parseExpr
    ]
  where
    prefExt  = try $ reserved "fun" *> reserved "external"
    prefFunC = try $ reserved "fun" *> reserved "comp" *> optionMaybe parseRange
    prefFun  = reserved "fun"
    prefLetC = try $ reserved "let" *> reserved "comp" *> optionMaybe parseRange
    prefLet  = reserved "let"

-- > <struct> ::= "struct" IDENT "=" "{" (IDENT ":" <base-type>)*";" "}"
parseStruct :: BlinkParser (GStructDef SrcTy)
parseStruct = do
    reserved "struct"
    x <- identifier
    reservedOp "="
    braces $ StructDef x <$> parseField `sepBy` semi
  where
    parseField = (,) <$> identifier <* colon <*> parseBaseType

-- | Like `parseVarBind` but for computation types
--
-- > <cvar-bind> ::= IDENT | "(" IDENT ":" <comp-base-type> ")"
parseCVarBind :: BlinkParser (GName SrcCTy)
parseCVarBind = choice
    [ withPos mkName <*> identifier
    , parens $
      withPos mkNameTy <*> identifier <* symbol ":" <*> parseCompBaseType
    ] <?> "variable binding"
  where
    mkName   p i    = toName i p SrcCTyUnknown
    mkNameTy p i ty = toName i p ty

-- | Parameters to a (comp) function
--
-- > <comp-params> ::= "(" (IDENT ":" (<base-type> | <comp-base-type>))*"," ")"
--
-- (<base-type> comes from the expr parser; <comp-base-type> is defined here).
compParamsParser :: BlinkParser [GName (CallArg SrcTy SrcCTy)]
compParamsParser = parens $ sepBy paramParser (symbol ",")
  where
    paramParser = withPos mkParam <*> identifier <* colon <*> parseType
    parseType   = choice [ CAExp  <$> parseBaseType
                         , CAComp <$> parseCompBaseType
                         ] <?> "computation parameter type"

    mkParam p x mty = toName x p mty

-- | Computation type
--
-- > <comp-base-type> ::= "ST" ("T" | "C" <base-type>) <base-type> <base-type>
parseCompBaseType :: BlinkParser SrcCTy
parseCompBaseType = choice
    [ mkCTy <$ reserved "ST" <*> parse_idx <*> parseBaseType <*> parseBaseType
    , parens parseCompBaseType
    ] <?> "computation type"
  where
    parse_idx = choice
      [ Nothing <$ reserved "T"
      , Just    <$ reserved "C" <*> parseBaseType
      , parens parse_idx
      ] <?> "computation type index"

    mkCTy Nothing   ti ty = SrcCTyKnown $ CTTrans ti ty
    mkCTy (Just tv) ti ty = SrcCTyKnown $ CTComp tv ti ty

-- | Smart constructor for LetDecl
--
-- Comparable to `eLetDecl`.
cLetDecl :: LetDecl -> SrcComp -> SrcComp
cLetDecl (LetDeclStruct   p sdef)     = cLetStruct p sdef
cLetDecl (LetDeclERef     p (x, e))   = cLetERef   p x e
cLetDecl (LetDeclExpr     p x e)      = cLetE      p x AutoInline e
cLetDecl (LetDeclComp     p h x c)    = cLet       p x    (mkVectComp c h)
cLetDecl (LetDeclFunComp  p h x ps c) = cLetFunC   p x ps (mkVectComp c h)
cLetDecl (LetDeclFunExpr  p fn ps e)  = cLetHeader p (mkFunDefined  p fn ps e)
cLetDecl (LetDeclExternal p x ps ty)  = cLetHeader p (mkFunExternal p fn ps ty)
  where
    fn = toName x p SrcTyUnknown

mkVectComp :: SrcComp -> Maybe (Int,Int) -> SrcComp
mkVectComp sc Nothing  = sc
mkVectComp sc (Just h) = cVectComp (compLoc sc) h sc

cunit :: Maybe SourcePos -> SrcComp
cunit p = cReturn p ForceInline (eunit p)

letDeclName :: LetDecl -> String
letDeclName (LetDeclERef _ (nm, _))     = show nm
letDeclName (LetDeclStruct _ sdef)      = struct_name sdef
letDeclName (LetDeclExternal _ nm _ _)  = nm
letDeclName (LetDeclFunComp _ _ nm _ _) = show nm
letDeclName (LetDeclFunExpr _ nm _ _)   = show nm
letDeclName (LetDeclComp _ _ nm _)      = show nm
letDeclName (LetDeclExpr _ nm _)        = show nm

{-------------------------------------------------------------------------------
  Dealing with the environment extension necessary to parse calls to
  computation functions
-------------------------------------------------------------------------------}

class ExtractParseEnv a where
  extractParseEnv :: a -> ParseCompEnv

instance ExtractParseEnv LetDecl where
  extractParseEnv (LetDeclFunComp _ _ x ps _) = [(x, ps)]
  extractParseEnv _                           = []

instance ExtractParseEnv Command where
  extractParseEnv (CmdDecl decl) = extractParseEnv decl
  extractParseEnv _              = []

-- | Like `>>=`, but extend the environment with new bindings
extBind :: ExtractParseEnv a
        => BlinkParser a -> (a -> BlinkParser b) -> BlinkParser b
extBind x f = x >>= \a -> extendParseEnv (extractParseEnv a) $ f a

infixl 1 `extBind`  -- Same precedence as (>>=)

-- | Like `sepEndBy1`, but using environment extention
extSepEndBy1 :: ExtractParseEnv a
          => BlinkParser a -> BlinkParser () -> BlinkParser [a]
extSepEndBy1 p sep = go
  where
    go   = p `extBind` \a -> (a:) <$> more
    more = sep *> (go <|> return [])

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

-- | Optional type annotation
--
-- > <type-ann> ::= "[" <base-type> "]"
optTypeAnn :: BlinkParser SrcTy
optTypeAnn = mkTypeAnn <$> optionMaybe (brackets parseBaseType)
  where
    mkTypeAnn Nothing  = SrcTyUnknown
    mkTypeAnn (Just t) = t

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
    mkFlag <$> optionMaybe (reservedOp "!") <*> parseRange
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

-- | Shorthand for @<inl-ann>?@
-- > <inl-ann> ::= "forceinline" | "autoinline" | "noinline"
optInlAnn :: BlinkParser ForceInline
optInlAnn = choice
   [ NoInline    <$ reserved "noinline"
   , ForceInline <$ reserved "forceinline"
   , AutoInline  <$ reserved "autoinline"
   , return AutoInline
   ]
