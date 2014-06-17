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
 
module BlinkParseExpr where

import AstExpr
import AstComp ( CTy0, CallArg (..) )

import Text.Parsec
import qualified BlinkToken as P
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Pos ( newPos )

import Data.Maybe ( fromMaybe, fromJust ) 

import Data.Char ( isSpace )

import Control.Monad.Trans
import Control.Monad.Reader.Class

import PpExpr 

import Eval ( evalInt ) 


unops
  = [ "-", "~" ]

binops 
  = [ "**", "*", "/", "%", "+", "-", "<<", ">>", "<"
    , ">", ">=", "<=", "&", "^", "|", "==", "!=", "&&", "||"
    , ">>>", "|>>>|"
    ]

blinkReservedNames 
  = [ -- Computation language keywords
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

debugParse :: SourcePos -> IO () -> BlinkParser ()
debugParse p action
 = liftIO $ do { putStrLn $ "Debug" 
               ; putStrLn $ "Position:" ++ show p
               ; action }

blinkStyle :: P.GenLanguageDef String BlinkParseState BlinkParseM
blinkStyle 
  = emptyDef { P.commentStart     = "{-"
             , P.commentEnd       = "-}"
             , P.commentLine      = "--"
             , P.nestedComments   = True
             , P.identStart       = letter
             , P.identLetter      = alphaNum <|> oneOf "_'"
             , P.opStart          = oneOf $ map head allops
             , P.opLetter         = oneOf $ concatMap tail allops
             , P.reservedNames    = blinkReservedNames
             , P.reservedOpNames  = allops } -- Why not put all of them in?
  where allops      = unops ++ binops ++ reservedops
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


-- Blink lexer
blinkLexer :: P.GenTokenParser String BlinkParseState BlinkParseM
blinkLexer  = P.makeTokenParser blinkStyle

identStart = P.identStart blinkStyle
identifier     =  P.identifier blinkLexer
reserved :: String -> BlinkParser ()
reserved x =  P.reserved blinkLexer x
operator       =  P.operator blinkLexer
reservedOp :: String -> BlinkParser ()
reservedOp x   =  P.reservedOp blinkLexer x
charLiteral    =  P.charLiteral blinkLexer
stringLiteral  =  P.stringLiteral blinkLexer
natural        =  P.natural blinkLexer
integer        =  P.integer blinkLexer
float          =  P.float blinkLexer 
naturalOrFloat =  P.naturalOrFloat blinkLexer
decimal        =  P.decimal blinkLexer
hexadecimal    =  P.hexadecimal blinkLexer
octal          =  P.octal blinkLexer
symbol :: String -> BlinkParser String
symbol x       =  P.symbol blinkLexer x
lexeme :: BlinkParser a -> BlinkParser a
lexeme x       =  P.lexeme blinkLexer x
whiteSpace     =  P.whiteSpace blinkLexer
parens :: BlinkParser a -> BlinkParser a
parens x =  P.parens blinkLexer x
braces :: BlinkParser a -> BlinkParser a
braces x =  P.braces blinkLexer x
angles :: BlinkParser a -> BlinkParser a
angles x =  P.angles blinkLexer x
brackets :: BlinkParser a -> BlinkParser a
brackets x =  P.brackets blinkLexer x
semi           =  P.semi blinkLexer
comma          =  P.comma blinkLexer
colon          =  P.colon blinkLexer
dot            =  P.dot blinkLexer
semiSep   x    =  P.semiSep blinkLexer x
semiSep1  x    =  P.semiSep1 blinkLexer x
commaSep  x    =  P.commaSep blinkLexer x
commaSep1 x    =  P.commaSep1 blinkLexer x


-- The Blink parser
-- (1) reads strings 
-- (2) maintains a BlinkParseState (unit for now)
-- (3) runs in the IO monad (for debugging)

type BlinkParseState = Int -- Keeps track of nesting of let-bound definitions
                           -- to avoid ill-nestings from file includes.

-- We need environment of defined functions to intelligently parse applications
type ParseCompEnv     = [(Name,[(Name, CallArg Ty CTy0)])]
newtype BlinkParseM a = BlinkParseM { runParseM :: ParseCompEnv -> IO a }
type BlinkParser a    = ParsecT String BlinkParseState BlinkParseM a 

instance Monad BlinkParseM where 
  (>>=) (BlinkParseM f) g 
     = BlinkParseM (\env -> 
         do { r <- f env; runParseM (g r) env })
  return v = BlinkParseM (\_env -> return v)

instance MonadIO BlinkParseM where 
  liftIO comp = BlinkParseM (\_ -> comp)

instance MonadReader ParseCompEnv BlinkParseM where
  ask = BlinkParseM (\env -> return env)
  local upd (BlinkParseM f) = BlinkParseM (\env -> f (upd env))
  reader f = BlinkParseM (\env -> return (f env)) 

extendParseEnv :: ParseCompEnv -> BlinkParser a -> BlinkParser a
extendParseEnv penv action
  = local (\env -> penv ++ env) action

getParseEnv :: BlinkParser ParseCompEnv
getParseEnv = ask 


-- Utilities
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mkNameFromPos :: Maybe String  -- optional source name 
              -> SourcePos     -- source position 
              -> Maybe Ty      -- optional source type annotation
              -> Name
mkNameFromPos mb_src_nm spos mb_ty
  = toName (fromMaybe uniq mb_src_nm) (Just spos) mb_ty
  where uniq = "_t" ++ ln ++ "_" ++ col
        ln   = show (sourceLine spos)
        col  = show (sourceColumn spos)


leftM :: Monad m => a -> m (Either a b)
leftM x = return (Left x)

rightM :: Monad m => b -> m (Either a b)
rightM x = return (Right x)


-- Parsing types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parseBaseType :: BlinkParser Ty
parseBaseType 
  = choice [ parens whiteSpace    >> return TUnit
           , reserved "bit"       >> return TBit 

           , reserved "int"       >> return tint
           , reserved "int8"      >> return tint8
           , reserved "int16"     >> return tint16
           , reserved "int32"     >> return tint32

           , reserved "double"    >> return (TDouble Full)
           , reserved "bool"      >> return TBool

           , reserved "complex"   >> return (TStruct complex32TyName)
           , reserved "complex8"  >> return (TStruct complex8TyName)
           , reserved "complex16" >> return (TStruct complex16TyName)
           , reserved "complex32" >> return (TStruct complex32TyName) 

           , reserved "struct"    >> parse_struct_cont
           , reserved "arr"       >> parse_arr_cont

           , parens parseBaseType

           ] <?> "expression type"
  where 
     parse_struct_cont :: BlinkParser Ty
     parse_struct_cont 
       = do { t <- identifier <?> "struct name"
            ; return $ TStruct t }
     parse_arr_cont :: BlinkParser Ty
     parse_arr_cont 
       = choice 
           [ do { res <- brackets int_or_length
                ; xPos <- getPosition
                ; t <- parseBaseType
                ; case res of
                    Left n  -> 
                      let i = fromIntegral n 
                      in return (TArr (Literal i) t)
                    Right x -> 
                      let nm = mkNameFromPos (Just x) xPos Nothing
                      in return (TArr (NArr nm) t) }
           , do { xPos <- getPosition
                ; t <- parseBaseType
                ; let nm = mkNameFromPos Nothing xPos (Just tint) 
                ; return $ TArr (NVar nm 0) t }
           ] <?> "array range and its base type"
       where 
         int_or_length 
           = choice [ reserved "length" >> parens identifier >>= rightM
                    , int_or_length_exp
                    ] <?> "array length description"

int_or_length_exp
  = do { e <- parseExpr <?> "expression"
       ; case evalInt e of 
           Just i  -> leftM i
           Nothing -> parserFail "Non-constant array length expression."
       }

-- Parse a variable, possibly with a type annotation
parseVarBind :: BlinkParser Name
parseVarBind
  = do { pos <- getPosition
       ; choice  
          [ do { i <- identifier
               ; return $ mkNameFromPos (Just i) pos Nothing }
          , parens $ 
            do { i <- identifier
               ; symbol ":"
               ; ty <- parseBaseType
               ; return $ mkNameFromPos (Just i) pos (Just ty)
               }
          ] <?> "variable binding"
       }


{- Parsing statements
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   stmts ::= sepEndBy stmt (symbol ";")
   stmt  ::= assignment | return expr | f(....) | for 
           | while | iter | if e then stmts [else stmts] | etc ...
           | let x = e;                                              
-}

parseStmts :: BlinkParser SrcExp
parseStmts
  = do { es <- comb parseStmt (optional semi) 
       ; return $ fromJust (fold_exp es) 
       }
  where fold_exp [e]    = Just (e Nothing)
        fold_exp (e:es) = Just (e (fold_exp es))
        fold_exp []     = error "Can't happen"

        comb = sepEndBy1


eunit p = eVal (Just p) () VUnit

mkoptseq :: SourcePos -> SrcExp -> Maybe SrcExp -> SrcExp
mkoptseq p e1 Nothing   = e1
mkoptseq p e1 (Just e2) = eSeq (Just p) () e1 e2

-- Assumes we have already read the head (function) of the call
parseCallArgs :: String 
              -> (SourcePos -> String -> [SrcExp] -> BlinkParser a)
              -> BlinkParser a
parseCallArgs x combine
  = do { p <- getPosition
       ; es <- parens $ sepBy parseExpr comma
       ; combine p x es }


combineAsCall :: Monad m => SourcePos -> String -> [SrcExp] -> m SrcExp
combineAsCall p fn args 
  = let f = eVar (Just p) () (mkNameFromPos (Just fn) p Nothing)
    in return $ eCall (Just p) () f args

combineAsCallOrCast :: Monad m => SourcePos -> String -> [SrcExp] -> m SrcExp
-- Create either a cast or a call since they share the same source syntax 
combineAsCallOrCast p x args
  | x == "int"       = assert_singleton args (cast tint)
  | x == "bit"       = assert_singleton args (cast TBit)
  | x == "double"    = assert_singleton args (cast tdouble)
  | x == "int32"     = assert_singleton args (cast tint32)
  | x == "int16"     = assert_singleton args (cast tint16)
  | x == "int8"      = assert_singleton args (cast tint8)
  | x == "complex"   = assert_singleton args (cast tcomplex)
  | x == "complex8"  = assert_singleton args (cast tcomplex8)
  | x == "complex16" = assert_singleton args (cast tcomplex16)
  | x == "complex32" = assert_singleton args (cast tcomplex32)
  | otherwise        = combineAsCall p x args
  where cast t x = eUnOp (Just p) () (Cast t) x

assert_singleton [e] action = return (action e)
assert_singleton _x _action = fail "Expecting only one argument!"





-- Assumes print/println keyword has already been parsed
-- Bool == true means println, otherwise it is print
parsePrint :: Bool -> BlinkParser SrcExp
parsePrint b
  = do { p <- getPosition
       ; es <- sepBy parseExpr comma
       ; return $ makePrint p b es }
    where makePrint p b (h:t) = 
            eSeq (Just p) () (ePrint (Just p) () False h) (makePrint p b t)
          makePrint p b [] = 
            ePrint (Just p) () b (eVal (Just p) () (VString ""))


-- Assumes we have already read the first variable of the lhs 
-- of the assignment, e.g x.f1.f3[35] := 42;
-- In this example we need to parse the rest, i.e. ".f1.f3[35] := 42; 
parseAssign :: SrcExp -> BlinkParser SrcExp
parseAssign e 
  = do { p <- getPosition
       ; choice [ do { symbol ":=" 
                     ; p <- getPosition 
                     ; erhs <- parseExpr 
                     ; return $ eAssign (Just p) () e erhs
                     }
                , do { symbol "."
                     ; y <- identifier
                     ; p <- getPosition 
                     ; parseAssign $ eProj (Just p) () e y
                     }
                , do { (estart,len) <- brackets rangeParser
                     ; symbol ":="
                     ; p <- getPosition 
                     ; erhs <- parseExpr
                     ; return $ eArrWrite (Just p) () e estart len erhs
                     }
                ] <?> "assignment"
        }
rangeParser :: BlinkParser (SrcExp, LengthInfo)
rangeParser
  = choice [ try intervalParser
           , do { e <- parseExpr 
                ; return (e, LISingleton) 
                }
           ] <?> "range"



genIntervalParser :: BlinkParser (SrcExp, SrcExp) 
-- A generalized interval parser 
-- Returns (start,length)
genIntervalParser 
  = choice [ try $ 
             do { p <- getPosition
                ; from <- integer
                ; colon
                ; to <- integer
                ; let start = fromIntegral from
                ; let len = (fromIntegral to) - (fromIntegral from) + 1
                ; return (eVal (Just p) () (VInt start),
                            eVal (Just p) () (VInt len))  
                }
           , do { startPos <- getPosition
                ; start <- parseExpr
                ; comma
                ; len <- parseExpr
                ; return (start, len)
                }
           ]

intervalParser :: BlinkParser (SrcExp, LengthInfo)
intervalParser
  = choice [ try $ 
             do { p <- getPosition
                ; from <- integer
                ; colon
                ; to <- integer
                ; let start = fromIntegral from
                ; let len = (fromIntegral to) - (fromIntegral from) + 1
                ; return (eVal (Just p) () (VInt start), LILength len) 
                }
           , do { startPos <- getPosition
                ; start <- parseExpr
                ; comma
                ; len <- integer
                ; return (start, LILength (fromIntegral len)) 
                }
           ]

type StmtCont = Maybe SrcExp -> SrcExp 

parseFor :: BlinkParser () -> BlinkParser UnrollInfo
parseFor for_reserved
  = choice [ for_reserved >> return AutoUnroll 
           , reserved "unroll"   >> for_reserved >> return Unroll
           , reserved "nounroll" >> for_reserved >> return NoUnroll 
           ]

parseStmt :: BlinkParser StmtCont
parseStmt
  = do { startPos <- getPosition
       ; choice (parse_stmt_choices startPos) <?> "statement" }
  where
    parse_stmt_choices :: SourcePos -> [BlinkParser StmtCont]
    parse_stmt_choices p 
        = [ do { reserved "let" 
               ; x <- parseVarBind
               ; symbol "=" 
               ; e <- parseExpr 
               ; scont <- optionMaybe $ 
                          do { reserved "in" 
                             ; parseStmt }
               ; case scont of 
                   Just r -> 
                     let k m = eLet (Just p) () x e (r m)
                     in return k
                   Nothing ->
                     let k m = eLet (Just p) () x e (fromMaybe (eunit p) m) 
                     in return k
               }

          , do { ui <- parseFor (reserved "for")
               ; k <- parseVarBind
               ; reserved "in"

               ; (estart,elen) <- brackets genIntervalParser

               ; ebody <- parseStmtBlock <?> "for loop body"
               ; return $ 
                 mkoptseq p (eFor (Just p) () ui k estart elen ebody)
               }

          , do { reserved "while"
               ; econd <- parens parseExpr 
               ; ebody <- parseStmtBlock <?> "while loop body"
               ; return $ 
                 mkoptseq p (eWhile (Just p) () econd ebody)
               }

          , do { reserved "if"
               ; e <- parseExpr 
               ; reserved "then" 
               ; e1 <- parseStmtBlock <?> "if branch"
               ; e2 <- parse_if_cont
               ; return $ 
                 mkoptseq p (eIf (Just p) () e e1 e2)
               }

          , do { reserved "return" 
               ; e <- parseExpr
               ; return $ mkoptseq p e }

          , do { reserved "print"
               ; e <- parsePrint False
               ; return $ mkoptseq p e
               }

          , do { reserved "println"
               ; e <- parsePrint True
               ; return $ mkoptseq p e
               }

          , do { reserved "error"
               ; s <- stringLiteral
               ; return $ mkoptseq p (eError (Just p) () s) 
               }
          , parse_call_or_assignment 
          ] 

    parse_call_or_assignment :: BlinkParser StmtCont
      = do { p <- getPosition
           ; x <- identifier <?> "variable or function"
           ; e <- choice 
                    [ parseCallArgs x combineAsCall
                    , parseAssign $ 
                      eVar (Just p) () $ 
                      mkNameFromPos (Just x) p Nothing
                    ] <?> "call or assignment"

           ; return $ mkoptseq p e
           }

    parse_if_cont :: BlinkParser SrcExp
      = choice [ do { notFollowedBy $ reserved "else" 
                    ; p <- getPosition 
                    ; return (eunit p) }
               , do { reserved "else" 
                    ; parseStmtBlock <?> "else branch"
                    }
               ]

parseStmtBlock :: BlinkParser SrcExp
parseStmtBlock
  = choice [ braces parseStmts
           , do { s <- parseStmt 
                ; return (s Nothing) 
                } 
           ]



-- Parsing expressions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- expr  ::= unops | binops | atomic_expr
-- value ::= intlit | boollit | bitlit | () | doublelit | { arrayvalinits }
-- atomic_expr  ::= ( expr ) | struct_init 
--                | deref_seq | call_or_cast | just_var | value
--                | if e1 then e2 else e3
--                | 
-- struct_init  ::= tyname { inits }
-- deref_seq    ::= deref_seq.f | deref_seq[range] | x
-- call_or_cast ::= f(expr) | tyname(expr)


parseScalarValue :: BlinkParser Val
parseScalarValue
  = choice
      [ try $ parens parseScalarValue
      , reserved "true"     >> return (VBool True )
      , reserved "false"    >> return (VBool False)
      , reserved "'0"       >> return (VBit  False)
      , reserved "'1"       >> return (VBit  True )
      , parens whiteSpace   >> return (VUnit      )
      , try $ do { f <- float
                 ; return (VDouble Full f) }
      , try $ do { s <- stringLiteral
                 ; return (VString s) 
                 }
      , do { i <- integer
           -- ; notFollowedBy identStart <?> ("end of " ++ show i)
           ; return (VInt (fromIntegral i)) 
           } 
      ]
    
parseValue :: BlinkParser SrcExp
parseValue
  = do { p <- getPosition 
       ; choice
          [ do { v <- parseScalarValue
               ; return $ eVal (Just p) () v }
          , do { varr <- braces (sepBy parseScalarValue comma)
               ; return (eValArr (Just p) () varr) }
          ] <?> "value"
       }

parseWithVarOnHead :: BlinkParser SrcExp
parseWithVarOnHead
  = do { x <- identifier
       ; p <- getPosition 
       ; let xexp = eVar (Just p) () (mkNameFromPos (Just x) p Nothing)
       ; choice [ do { notFollowedBy (symbol "{")
                     ; notFollowedBy (symbol ".")
                     ; notFollowedBy (symbol "[")
                     ; notFollowedBy (symbol "(")
                     ; return xexp }
                , parse_struct_inits p x  -- struct initialization eg: { ... }  
                , parse_deref_seq xexp    -- dereference sequence  eg: .x 
                , parseCallArgs x combineAsCallOrCast 
                                          -- call or cast          eg: (e1,e2)
                ]
       }
  where
    parse_struct_inits p x 
      = do { tfs <- braces (sepBy1 parse_init semi)
           ; return (eStruct (Just p) () (patch_prim_struct x) tfs) }
    patch_prim_struct x | x == "complex" = complex32TyName
                        | otherwise      = x 
    parse_init
      = do { fn <- identifier
           ; symbol "=" 
           ; fe <- parseExpr
           ; return (fn,fe) 
           }

    parse_deref_seq e 
      = choice [ do { symbol "."
                    ; y <- identifier
                    ; p <- getPosition
                    ; let pe = eProj (Just p) () e y
                    ; choice [ try (parse_deref_seq pe)
                             , return pe 
                             ]
                    }
               , do { (est,len) <- brackets rangeParser
                    ; p <- getPosition
                    ; let pe = eArrRead (Just p) () e est len
                    ; choice [ try (parse_deref_seq pe)
                             , return pe
                             ]
                    }
               ]


parseTerm  :: BlinkParser SrcExp
parseTerm 
  = choice [ parens $ 
             do { p <- getPosition 
                ; choice [ try parseExpr
                         , do { whiteSpace 
                              ; return (eVal (Just p) () VUnit)
                              } ] }
           , parseValue
           , parseWithVarOnHead
           , parse_let
           , parse_cond
           ] <?> "expression"
  where 

    parse_let 
      = do { reserved "let"
           ; p <- getPosition
           ; nm <- parseVarBind 
           ; symbol "=" 
           ; e1 <- parseExpr
           ; reserved "in"
           ; e2 <- parseExpr 
           ; return $ eLet (Just p) () nm e1 e2
           }

    parse_cond 
      = do { p <- getPosition
           ; reserved "if"
           ; e <- parseExpr 
           ; reserved "then"
           ; e1 <- parseExpr
           ; reserved "else"
           ; e2 <- parseExpr
           ; return (eIf (Just p) () e e1 e2) 
           }

parseExpr :: BlinkParser SrcExp
  = buildExpressionParser exprOpTable parseTerm <?> "expression"

exprOpTable 
  = [ 
          [ prefixUnOp (reservedOp "-") Neg
          , prefixUnOp (reserved "not") Not
          , prefixUnOp (reservedOp "~") BwNeg 
          ]   

        , [ prefixUnOp (reserved "length") ALength ]


        , [ infixBinOp (reservedOp "**") Expon AssocLeft
          , infixBinOp (reservedOp "*")  Mult  AssocLeft
          , infixBinOp (reservedOp "/")  Div   AssocLeft
          , infixBinOp (reservedOp "%")  Rem   AssocLeft 
          ]

        , [ infixBinOp (reservedOp "+" ) Add AssocLeft
          , infixBinOp (reservedOp "-" ) Sub AssocLeft 
          ]

        , [ infixBinOp (reservedOp "<<") ShL AssocLeft
          , infixBinOp (reservedOp ">>") ShR AssocLeft 
          ]

        , [ infixBinOp (reservedOp "<" ) Lt  AssocLeft
          , infixBinOp (reservedOp "<=") Leq AssocLeft
          , infixBinOp (reservedOp ">" ) Gt  AssocLeft
          , infixBinOp (reservedOp ">=") Geq AssocLeft 
          ]

        , [ infixBinOp (reservedOp "&" ) BwAnd AssocLeft ]

        , [ infixBinOp (reservedOp "^" ) BwXor AssocLeft ]

        , [ infixBinOp (reservedOp "|" ) BwOr AssocLeft  ]

        , [ infixBinOp (reservedOp "==") Eq  AssocLeft
          , infixBinOp (reservedOp "!=") Neq AssocLeft 
          ] 

        , [ infixBinOp (reservedOp "&&") And AssocLeft ]
        , [ infixBinOp (reservedOp "||") Or AssocLeft  ] 

   ]


prefixUnOp parseop op 
  = Prefix action
  where action = do { p <- getPosition
                    ; parseop
                    ; return (eUnOp (Just p) () op) }

infixBinOp parseop op 
  = Infix action
  where action 
          = do { p <- getPosition
               ; parseop
               ; return (\x y -> eBinOp (Just p) () op x y) }


{- New Syntax
   ~~~~~~~~~~

comp 
  ::= comp >>> comp | comp |>>>| comp
    | f(args)
    | repeat { commands }    | repeat comp
    | times ... { commands } | times ... comp
    | seq { commands }
    | take
    | takes expr
    | emit expr
    | emits expr
    | read | write 
    | do { stmts }
    | let binding in comp

commands := sepBy1 ";" command  

command 
  ::= (x <- comp)
    | comp
    | let binding in command


expr ::= x | unops | binops | values | f(...) 
       | do { stmts } 
       | if e1 then expr else expr
       | lut e 
       | let binding in expr
       | (expr)

stmts ::= sepBy1 ";" stmt
stmt  ::= assignments | return expr | f(....) | for | while | iter | if e then stmts [else stmts]

program ::= defs; main = comp

defs := sequence def

def := let [qual] f(args) [opt_return_ty] = 
              local_decls_inits; 
              (expr | comp)

     | let external f(args) return_ty 
     | struct struct_def



-}
