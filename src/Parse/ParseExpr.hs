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
{-# LANGUAGE GADTs, TemplateHaskell, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}

module ParseExpr where

import AstExpr

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

-- import Debug.Trace ( trace )


tynamePos s mbty pos 
  = MkName { name    = s
           , uniqId  = s
           , nameLoc = Just pos 
           , mbtype  = mbty }



parsingErr msg =
  "Parsing error, expected: " ++ msg

clearErrs :: (Monad m) => ParsecT [Char] u m [Char]
clearErrs =
  do inp <- getInput
     setInput ('x':inp)
     anyToken
     return inp

myfail msg =
  do inp <- clearErrs
     let msg' = parsingErr msg ++ "\nbut got: " ++ take 30 inp
     fail msg'

unops =
  [ "-"
  , "~" ]

binops =
  [ "**"
  , "*"
  , "/"
  , "%"
  , "+"
  , "-"
  , "<<"
  , ">>"
  , "<"
  , ">"
  , ">="
  , "&"
  , "^"
  , "|"
  , "=="
  , "!="
  , "&&"
  , "||" ]

ops = unops ++ binops

opStartLetters = map head ops

opTailLetters = concatMap tail ops

wplStyle :: LanguageDef st
wplStyle =
  emptyDef { P.commentStart   = "{-"
           , P.commentEnd     = "-}"
           , P.commentLine    = "--"
           , P.nestedComments = True
           , P.identStart     = letter
           , P.identLetter    = alphaNum <|> oneOf "_"
           , P.opStart        = oneOf opStartLetters 
           , P.opLetter       = oneOf opTailLetters 
           , P.reservedOpNames= [] -- not actually used by Token package
           , P.reservedNames  = ["let","in","if","then","else","true","false","input", "read", "write",
                                 "emit1","emit","emits","return","interleave","take1","take","until","times",
                                 "repeat","iter","filter","flatten","not","map","var","bperm", 
                                 "do","tt","int","int8","int16","int32","unit","bool","word8","word16","word32",
                                 "complex", "complex16", "complex32", "complex8",
                                 "for","lut","letexternal","standalone", "struct" ]
           , P.caseSensitive  = True }

lexer = P.makeTokenParser wplStyle

identifier = P.identifier lexer

reserved = P.reserved lexer

operator = P.operator lexer

reservedOp = P.reservedOp lexer

charLiteral = P.charLiteral lexer

stringLiteral = P.stringLiteral lexer

natural = P.natural lexer

integer = P.integer lexer

float = P.float lexer

naturalOrFloat = P.naturalOrFloat lexer

decimal = P.decimal lexer

hexadecimal = P.hexadecimal lexer

octal = P.octal lexer

symbol = P.symbol lexer

lexeme = P.lexeme lexer

whiteSpace = P.whiteSpace lexer

parens = P.parens lexer

braces = P.braces lexer

angles = P.angles lexer

brackets = P.brackets lexer

semi = P.semi lexer

comma = P.comma lexer

colon = P.colon lexer

dot = P.dot lexer

semiSep = P.semiSep lexer

semiSep1 = P.semiSep1 lexer

commaSep = P.commaSep lexer

commaSep1 = P.commaSep1 lexer

liftBinOp op pos x y = EBinOp op (toExpPos () pos x) (toExpPos () pos y)

liftAssign x y = EAssign (toExp () x) (toExp () y)


rangeParser = choice [ try (do { (start,len) <- intervalParser
                               ; return (start, len)
                               })
                     , (exprParser >>= \(e :: Exp0 ()) -> return (e,LISingleton))
                     ]



data IdInfo = MkIdInfo { ids_head      :: Exp ()
                       , ids_fld_projs :: [String]
                       , ids_rng       :: Maybe (Exp (), LengthInfo) }

parseIdentifiers = 
  do { startPos <- getPosition
     ; xs <- sepBy1 identifier (symbol ".");

     ; mb_range <- option Nothing $ 
                   do { (e1 :: Exp0 (),r) <- brackets rangeParser
                      ; return $ Just (toExpPos () startPos e1, r) }

     ; return $ MkIdInfo { ids_head      = toExpPos () startPos (EVar (toName (head xs) Nothing Nothing))
                         , ids_fld_projs = tail xs
                         , ids_rng       = mb_range }
     }


intervalParser = 
  try (do { startPos <- getPosition
          ; from <- integer
          ; symbol ":"
          ; to <- integer
          ; let start = fromIntegral from
          ; let len = ((fromIntegral to)-(fromIntegral from)+1)
          ; return (EVal (VInt start), LILength (len))
          })
  <|>  do { startPos <- getPosition
          ; start <- exprParser
          ; symbol ","
          ; len <- integer
          ; return (start, LILength(fromIntegral len)) 
          }

mk_field_projs loc expr []   = expr
mk_field_projs loc hd (f:fs) = mk_field_projs loc (toExpPos () loc (EProj hd f)) fs


namePos s pos 
  = MkName { name    = s
           , uniqId  = s
           , nameLoc = Just pos 
           , mbtype  = Nothing }

-- Simple terms (exprParser,term):
-- t := (lhs := rhs) | dereferencing | f(t1...tn) | built-in-table-ones | values 
term = parens exprParser
        <|> try (do { ids_info <- parseIdentifiers
                    ; loc <- getPosition 
                    ; let projs_exp = mk_field_projs loc (ids_head ids_info) (ids_fld_projs ids_info)
                    ; symbol ":=" 
                    ; e1Pos <- getPosition
                    ; e1 <- exprParser
                    ; let rhs = toExpPos () e1Pos e1
                    ; case ids_rng ids_info of 
                       Nothing     -> return (EAssign projs_exp rhs)
                       Just (er,r) -> return (EArrWrite projs_exp er r rhs)
                   }) 

       <|> try (do { startPos <- getPosition
                   ; x <- identifier
                   ; esPos <- getPosition
                   ; es <- parens (sepBy exprParser (symbol ","))
                   ; return (ECall (toExpPos () startPos (EVar (toName x Nothing Nothing)))
                                   (map (toExpPos () esPos) es))
                   })
       
       <|> try (do { ids_info <- parseIdentifiers
                   ; loc <- getPosition
                   ; let projs_exp = mk_field_projs loc (ids_head ids_info) (ids_fld_projs ids_info)
                   ; case ids_rng ids_info of 
                       Nothing     -> return (unExp projs_exp)
                       Just (er,r) -> return (EArrRead projs_exp er r)
                   })

       <|> do { startPos <- getPosition
              ; exp <- braces arrayParser 
              ; return (EValArr exp) }
              
       <|> (try (float >>= \d -> return (EVal (VDouble Full d))))
       <|> (integer >>= \n -> return (EVal (VInt $ fromIntegral n)))
       <|> (reserved "true" >> return (EVal (VBool True)))
       <|> (reserved "false" >> return (EVal (VBool False)))
       <|> (reserved "'0" >> return (EVal (VBit False)))
       <|> (reserved "'1" >> return (EVal (VBit True)))       
       <|> (reserved "tt" >> return (EVal VUnit))
       <|> myfail "<exp>"

-- The reason exprParser is choice is because of the conflict between:
--    complex { ... inits ... }   // EStruct
-- And
--    complex(foo)                // Unop-cast
-- We need to try first if EStruct can work and then do the unop
-- thing.  That's a bit unsatisfactory, I wonder if casts are
-- justifiable as prefix ops.
exprParser 
  = choice [ try (do { startPos <- getPosition
                     ; x <- identifier
                     ; sdefs <- braces structInitsParser 
                     ; return $ EStruct (patch_prim_struct x) sdefs })
           , buildExpressionParser table term <?> "expression"
           ]
  where patch_prim_struct x | x == "complex" = complex32TyName
                            | otherwise      = x 

-- BOZIDAR: BEGIN - parsing constant array
arrterm =     braces arrayParser
       <|> (try (float >>= \d -> return [VDouble Full d]))
       <|> (integer >>= \n -> return [VInt $ fromIntegral n])
       <|> (reserved "true" >> return [VBool True])
       <|> (reserved "false" >> return [VBool False])
       <|> (reserved "'0" >> return [VBit False])
       <|> (reserved "'1" >> return [VBit True])       
       <|> (reserved "tt" >> return [VUnit])
       <|> myfail "<const>"

arrayParser = buildExpressionParser [[aInfixBinOp "," AssocLeft]] arrterm <?> "array"

aInfixBinOp s assoc =
  let g = do { symbol s
             ; return (aLiftBinOp)
             }
  in Infix g assoc

aLiftBinOp x y = (x ++ y)
-- BOZIDAR: END - parsing constant array


table = [ 
          [ prefixRes "int"       (Cast tint) 
          , prefixRes "bit"       (Cast TBit)
          , prefixRes "double"    (Cast tdouble)
          , prefixRes "int32"     (Cast tint32)
          , prefixRes "int16"     (Cast tint16)
          , prefixRes "int8"      (Cast tint8)

          , prefixRes "complex"   (Cast tcomplex)
          , prefixRes "complex8"  (Cast tcomplex8)
          , prefixRes "complex16" (Cast tcomplex16)
          , prefixRes "complex32" (Cast tcomplex32)

          ]

        , [ prefix "-" Neg
          , prefixRes "not" Not
          , prefix "~" BwNeg ]   

        , [ prefixRes "length" ALength ]

        , [ infixBinOpRes "**" Expon AssocLeft
          , infixBinOpRes "*" Mult AssocLeft
          , infixBinOp "/" Div AssocLeft
          , infixBinOp "%" Rem AssocLeft ]
        , [ infixBinOp "+" Add AssocLeft
          , infixBinOp "-" Sub AssocLeft ]
        , [ infixBinOpRes "<<" ShL AssocLeft
          , infixBinOpRes ">>" ShR AssocLeft ]
        , [ infixBinOpRes "<" Lt AssocLeft
          , infixBinOpRes "<=" Leq AssocLeft
          , infixBinOpRes ">" Gt AssocLeft
          , infixBinOpRes ">=" Geq AssocLeft ]
        , [ infixBinOpRes "&" BwAnd AssocLeft ]
        , [ infixBinOp "^" BwXor AssocLeft ]
        , [ infixBinOpRes "|" BwOr AssocLeft ]
        , [ infixBinOpRes "==" Eq AssocLeft
          , infixBinOp "!=" Neq AssocLeft ] 
        , [ infixBinOpRes "&&" And AssocLeft ]
        , [ infixBinOpRes "||" Or AssocLeft ] 
      ]

prefix s op =
  let g = do { pos <- getPosition
             ; symbol s
             ; return (EUnOp op . toExpPos () pos)
             }
  in Prefix g

prefixRes s op =
  let g = do { pos <- getPosition
             ; reserved s ; 
             ; notFollowedBy (whiteSpace >> symbol "{")
             ; return (EUnOp op . toExpPos () pos)
             }
  in Prefix g

infixBinOp s op assoc =
  let g = do { pos <- getPosition
             ; symbol s
             ; return (liftBinOp op pos)
             }
  in Infix g assoc

infixBinOpRes s op assoc =
  let g = do { pos <- getPosition
             ; reservedOp s
             ; return (liftBinOp op pos)
             }
  in Infix g assoc

parseBaseType =
      do { reserved "unit"; return TUnit }
  <|> do { reserved "bit"; return TBit   }

  <|> do { reserved "int"  ; return tint   }
  <|> do { reserved "int8" ; return tint8  }
  <|> do { reserved "int16"; return tint16 }
  <|> do { reserved "int32"; return tint32 }


  <|> try (do { reserved "fixed"
              ; n <- parens integer
              ; return (TDouble (Fixed (fromIntegral n)))})

  <|> do { xPos <- getPosition
         ; reserved "fixed"
         ; let ln  = show (sourceLine xPos)
               col = show (sourceColumn xPos)
               uniq_nm = namePos ("_t" ++ ln ++ "_" ++ col) xPos
         ; return (TDouble (Unknown uniq_nm)) }

  <|> do { reserved "double"
         ; return (TDouble Full)}
  <|> do { reserved "bool"
         ; return TBool }

      -- NB: Allow the parser to read complex, but internally make a complex32, just as we do for int
  <|> do { reserved "complex"
         ; return (TStruct complex32TyName) }

  <|> do { reserved "complex32"
         ; return (TStruct complex32TyName) 
         }

  <|> do { reserved "complex16"
         ; return (TStruct complex16TyName) 
         }

  <|> do { reserved "complex8"
         ; return (TStruct complex8TyName) 
         }

  <|> do { xPos <- getPosition
         ; reserved "arr"
         ; try (do { n <- brackets integer
                   ; t <- parseBaseType
                   ; return $ TArr (Literal (fromIntegral n)) t })
           <|> do {  x <- brackets ( symbol "length" >>  parens identifier) 
                  ; t <- parseBaseType
                  ; return $ TArr (NArr (namePos x xPos)) t }
           <|> do { t <- parseBaseType
                  -- Create unique name based on col and pos
                  ; let ln  = show (sourceLine xPos)
                        col = show (sourceColumn xPos)
                        uniq_nm = namePos ("_t" ++ ln ++ "_" ++ col) xPos
                  ; return $ TArr (NVar uniq_nm 0) t } }

  <|> do { xPos <- getPosition
         ; reserved "struct" 
         ; t <- identifier 
         ; return $ TStruct t }

  <|> myfail "<base-type>"      


parseStmt1 =
      do { startPos <- getPosition
         ; reserved "let"
         ;     do { xPos <- getPosition
                  ; x <- identifier
                  ; symbol "="
                  ; e1 <- parseStmt1
                  ; reserved "in"
                  ; es2 <- semiSep1 parseStmt1
                  ; return $
                    toExpPos
                    () startPos
                    (ELet (namePos x xPos) e1 (compose es2))
                  }
           <|> myfail "let <ident> = <exp> in <exps>"               
         }
  <|> try (do { startPos <- getPosition
         ; reserved "for"
         ; k <- annName
         ; reserved "in"
         ; e1 <- parseStmt1
         ; comma
         ; e2 <- parseStmt1   
         ; es2 <- braces (semiSep1 parseStmt1)
         ; return $
             toExpPos
             () startPos
             (EFor k e1 e2 (compose es2))
         })
  <|> do { startPos <- getPosition
         ; reserved "for"
         ;     try (do { k <- annName
                       ; comma         
                       ; v <- annName
                       ; reserved "in"
                       ; e1 <- parseStmt1
                       ; es2 <- braces $ semiSep1 parseStmt1
                       ; return $
                         toExpPos
                         () startPos
                         (EIter k v e1 (compose es2))
                       })
           <|> myfail "for <ident> in <start-exp>[, <len-exp>] { <exps> }"
         }

  <|> try (do { startPos <- getPosition
         ; reserved "if"
         ;     do { bePos <- getPosition
                  ; be <- exprParser
                  ; es1 <- braces (semiSep1 parseStmt1)
                  ; reserved "else"
                  ; es2 <- braces (semiSep1 parseStmt1)
                  ; return $
                    toExpPos
                    () startPos
                    (EIf (toExpPos () bePos be) (compose es1) (compose es2))
                  }
         })

  <|> do { startPos <- getPosition
         ; reserved "if"
         ;     do { bePos <- getPosition
                  ; be <- exprParser
                  ; es1 <- braces (semiSep1 parseStmt1)
                  ; return $
                    toExpPos
                    () startPos
                    (EIf (toExpPos () bePos be) (compose es1) (toExp () (EVal (VUnit))))
                  }
           <|> myfail "if <exp> { <exps> } [ else { <exps>} ]"
         }

  <|> do { startPos <- getPosition
         ; reserved "print"
         ;     do { ePos <- getPosition
                  ; e <- exprParser
                  ; return $
                    toExpPos
                    () startPos
                    (EPrint False (toExpPos () ePos e))
                  }
           <|> myfail "print <exp>"      
         }

  <|> do { startPos <- getPosition
         ; reserved "bperm"
         ;     do { ePos1 <- getPosition
                  ; e1 <- exprParser
                  ; ePos2 <- getPosition
                  ; e2 <- exprParser
                  ; return $
                    toExpPos
                    () startPos
                    (EBPerm (toExpPos () ePos1 e1) (toExpPos () ePos2 e2))
                  }
           <|> myfail "bperm <exp> <exp>"     
         }


  <|> do { startPos <- getPosition
         ; reserved "println"
         ;     do { ePos <- getPosition
                  ; e <- exprParser
                  ; return $
                    toExpPos
                    () startPos
                    (EPrint True (toExpPos () ePos e))
                  }
           <|> myfail "println <exp>"      
         }


  <|> do { startPos <- getPosition
         ; reserved "error"
         ;     do { ePos <- getPosition
                  ; str <- stringLiteral
                  ; return $
                    toExpPos
                    () startPos
                    (EError str)
                  }
           <|> myfail "error <exp>"      
         }


  <|> try (do { startPos <- getPosition
              ; x <- identifier
              ; sdefs <- braces structInitsParser 
              ; return $ toExpPos () startPos (EStruct x sdefs) })


  <|> do { startPos <- getPosition
         ; let e1Pos = startPos
         ; e1 <- exprParser 
         ; return $ toExpPos () startPos e1
         }

  <|> myfail "<stmt>"



structInitsParser = 
  try (do { tfs <- sepBy1 (do { fn <- identifier
                              ; symbol "=" 
                              ; epos <- getPosition 
                              ; fe <- exprParser
                              ; return (fn,toExpPos () epos fe) }) (symbol ";")
          ; return tfs })


-- Parse a variable, possibly with a type annotation
annName :: Parsec String () Name
annName =
      do { pos <- getPosition
         ; i <- identifier
         ; return (tynamePos i Nothing pos)
         }
  <|> do { pos <- getPosition
         ; parens $ do { i <- identifier
                       ; symbol ":"
                       ; ty <- parseBaseType
                       ; return (tynamePos i (Just ty) pos) } }

exprParserToplevel :: Parsec String () (Exp ())
exprParserToplevel = 
  do es <- semiSep1 parseStmt1 
     return $ compose es


compose (e : []) = e
compose (e : es') = toExp () (ESeq e (compose es')) 
compose [] = error "Impossible to compose empty list!"





composeVal (e : []) = e
composeVal (e : es') = toExp () (ESeq e (compose es')) 
composeVal [] = error "Impossible to compose empty list!"
