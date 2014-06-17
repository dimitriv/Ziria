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
module ParseComp where

import AstExpr
import AstComp
import ParseExpr

import qualified Data.Map as Map
import Text.Parsec


namePosTy (s,Nothing) pos 
 = MkName { name    = s
          , uniqId  = s
          , nameLoc = Just pos
          , mbtype  = Nothing  }

namePosTy (s,Just ty) pos 
  = MkName { name = s, uniqId = s
           , nameLoc = Just pos
           , mbtype = Just ty }



compParser =
      do { parens compParserToplevel }
  <|> do { startPos <- getPosition
         ; reserved "emit"
         ;     do { ePos <- getPosition
                  ; e <- exprParser
                  ; return (toCompPos () startPos $ Emit (toExpPos () ePos e))
                  }
           <|> myfail "emit <exp>"       
         }
  <|> do { startPos <- getPosition
         ; reserved "emits"
         ;     do { ePos <- getPosition
                  ; e <- exprParser  -- TODO: This has to match with array expression only
                  ; return (toCompPos () startPos $ Emits (toExpPos () ePos e))
                  }
           <|> myfail "emits <exp>"       
         }

  <|> do { startPos <- getPosition
         ; reserved "return"
         ;     do { ePos <- getPosition
                  ; e <- parens exprParserToplevel
                  ; return (toCompPos () startPos $ Return e)
                  }
           <|> myfail "return (<exp>;<exp>;...)"     
         }
  <|> do { startPos <- getPosition
         ; reserved "take1"
         ; return (toCompPos () startPos $ Take1)
         }
  <|> do { startPos <- getPosition
         ; reserved "take"
         ;     do { ePos <- getPosition
                  ; e <- exprParser
                  ; return (toCompPos () startPos $ Take (toExpPos () ePos e))
                  }
           <|> myfail "take <exp>"     
         }
  <|> do { startPos <- getPosition
         ; reserved "until"
         ;     do { ePos <- getPosition           
                  ; e <- exprParser
                  ; reserved "do"
                  ; c <- compParser
                  ; return (toCompPos () startPos $ Until (toExpPos () ePos e) c)
                  }
           <|> myfail "until <exp> do <comp>"      
         }
  <|> do { startPos <- getPosition
         ; reserved "times"
         ; let parseBound = choice 
                   [ try $ 
                     do { x <- identifier; symbol "="; e <- exprParser; return (x,e) }
                   , do { e <- exprParser; return ("_internal_count",e) } 
                   ]
         ;     do { ePos <- getPosition
                  ; (x,e) <- parseBound

                  ; reserved "do"  
                  ; c <- compParser
                  ; return (toCompPos () startPos $ Times AutoUnroll 
                                                          (toExpPos () ePos (EVal (VInt 0))) 
                                                          (toExpPos () ePos e) (namePos x ePos) c)
                  }
           <|> myfail "times <exp> do <comp>"               
         }
  <|> do { startPos <- getPosition
         ; reserved "repeat"
         ;     do { wdth <- brackets $ do { i <- integer
                                          ; symbol ","
                                          ; j <- integer
                                          ; return (fromIntegral i, fromIntegral j) 
                                          }
                  ; c    <- compParser
                  ; return (toCompPos () startPos $ Repeat (Just $ wdth) c)
                  }
           <|> do { c <- compParser
                  ; return (toCompPos () startPos $ Repeat Nothing c)
                  }
           <|> myfail (parsingErr "repeat [ \\[width\\] ] <comp>")
         }
  <|> do { reserved "map"
         ; ePos <- getPosition           
         ;     do { wdth <- brackets $ do { i <- integer
                                          ; symbol ","
                                          ; j <- integer
                                          ; return (fromIntegral i, fromIntegral j) 
                                          }
                  ; e    <- exprParser
                  ; return (toComp () $ Map (Just wdth) (toExpPos () ePos e))
                  }
           <|> do { e    <- exprParser 
                  ; return (toComp () $ Map Nothing (toExpPos () ePos e))
                  }
           <|> myfail (parsingErr "map [ \\[width\\] ] <exp>")
         }

  <|> do { startPos <- getPosition
         ; reserved "filter"
         ; ePos <- getPosition                      
         ; e <- exprParser
         ; return (toCompPos () startPos $ Filter (toExpPos () ePos e))
         }

  -- For now give the user the ability to say 'read' and 'write', in
  -- order to serialize the input or output. Hence using the
  -- predefined internal constants inFileBuf and outFileBuf (see
  -- AstExpr.hs) Later we should probably add the possibility of using
  -- custom-declared serialization queues.
  
  <|> do { startPos <- getPosition
         ; reserved "read"
         ; mty <- optionMaybe $ brackets parseBaseType
         ; let annty = case mty of Nothing -> RWNoTyAnn
                                   Just t  -> RWRealTyAnn t
         ; return (toCompPos () startPos $ (ReadSrc annty))
         }

  <|> do { startPos <- getPosition
         ; reserved "write"
         ; mty <- optionMaybe $ brackets parseBaseType
         ; let annty = case mty of Nothing -> RWNoTyAnn
                                   Just t  -> RWRealTyAnn t
         ; return (toCompPos () startPos $ (WriteSnk annty))
         }

  <|> do { startPos <- getPosition
         ; let xPos = startPos
         ; x <- identifier               
         ; choice [ try $ 
                    do { es <- parens (sepBy (try exprParser) (symbol ","))
                       ; return (toCompPos () startPos $
                           Call (toName x Nothing Nothing) (map (CAExp . toExp ()) es))
                       }
                  , return (toCompPos () startPos $ 
                    Var (namePos x xPos))
                  ]
         }

parseSeqOrPar startPos xComp =
      do { symbol ";"
         ; c2 <- compParserToplevel
         ; return (toCompPos () startPos $ Seq xComp c2)
         }
  <|> do { symbol "|>>>|"
         ; c2 <- compParserToplevel
         ; return (toCompPos () startPos $ Par (mkParInfo (AlwaysPipeline 0 0)) xComp c2)
         }
  <|> do { symbol ">>>"
         ; c2 <- compParserToplevel
         ; return (toCompPos () startPos $ Par (mkParInfo MaybePipeline) xComp c2)
         }
  <|> do { symbol ".>>>."
         ; c2 <- compParserToplevel
         ; return (toCompPos () startPos $ Par (mkParInfo NeverPipeline) xComp c2)
         }

identifierOrTypedIdentifier 
  = choice [ try (parens $ do { x <- identifier
                              ; colon
                              ; ty <- parseBaseType 
                              ; return (x,Just ty) })
           , (identifier >>= \x -> return (x, Nothing)) 
           ]


parseComps startPos =
    do { c1 <- choice [compParser, parens compParser]
       ;     parseSeqOrPar startPos c1
         <|> do { return c1 }
       }

compParserToplevel :: Parsec String () (Comp () ())
compParserToplevel = 
      -- Modified to enforce {} around then and else statements
      do { startPos <- getPosition
         ; reserved "if"
         ;     do { ePos <- getPosition
                  ; e <- exprParser
                  ; reserved "then"
                  ; c1 <- braces (compParserToplevel)
                  ; reserved "else"
                  ; c2 <- braces (compParserToplevel)
                  ;     do { symbol ";"
                           ; c3 <- compParserToplevel
                           ; let branch = (toCompPos () startPos $ Branch (toExpPos () ePos e) c1 c2)
                           ; let seq = (toCompPos () startPos $ Seq branch c3)
                           ; return seq
                           }
                    <|> do { return (toCompPos () startPos $ Branch (toExpPos () ePos e) c1 c2) }
                  }
           <|> myfail "if <exp> then {<comp>} else {<comp>}"
         }               
  <|> try (do { startPos <- getPosition
         ; let xPos = startPos
         ; x <- identifierOrTypedIdentifier
         ; notFollowedBy (symbol "(")
         ; let xName = namePosTy x xPos
         ; let xComp = toCompPos () xPos $ Var xName               
         ;     do { symbol "<-"
                  ; c1 <- compParser
                  ; symbol ";"
                  ; c2 <- compParserToplevel
                  ; return (toCompPos () startPos $ mkBind c1 (xName, c2))
                  }
           <|> parseSeqOrPar startPos xComp
           <|> do { return (toCompPos () startPos $ 
                    Var (namePosTy x xPos)) }
         })

  {- Parse a comp wrapped in an application of [standalone] -}
  <|> try (do { startPos <- getPosition
              ; reserved "standalone"
              ; c1 <- parens compParserToplevel
              ; let c1' = toCompPos () startPos $ Standalone c1
              ;    parseSeqOrPar startPos c1'
               <|> do { return c1' }
              })

  {- Parse a series of comps (possibly singleton) connected by [;] or [>>>], [.>>>.], etc. -}
  <|> do { startPos <- getPosition
         ; parseComps startPos
         }

  <|> do { startPos <- getPosition
         ; reserved "struct" 
         ; xPos <- getPosition
         ; x <- identifier
         ; symbol "="
         ; structdef <- braces (structDefParser x)
         ; reserved "in"
         ; c2 <- compParserToplevel
         ; return $ toCompPos () startPos (LetStruct structdef c2) }

  <|> do { startPos <- getPosition
         ; reserved "let"
         ; xPos <- getPosition
         ; x <- identifier
         ;     do {
                  ; params <- parens paramsParser
                  ; symbol "="
                  ; locls <- declsParser
                  ; ec <- (    try (do { e1 <- exprParserToplevel
                                       ; return $ Left e1
                                       })
                           <|> do { c1 <- compParserToplevel
                                  ; return $ Right c1
                                  }
                           <|> myfail "let <ident>(<params>) = (<exp>|<comp>) in <comp>")
                  ; reserved "in"
                  ; c2 <- compParserToplevel           
                  ; case ec of
                      Left e1 -> return $
                                  toCompPos () startPos
                                  (LetFun (namePos x xPos) 
                                   (toFunPos () startPos
                                    (MkFunDefined (namePos x xPos) params locls e1))
                                   c2)
                      Right c1 -> return $
                                  toCompPos () startPos
                                  (LetFunC (namePos x xPos) (map (\(x,t) -> (x, CAExp t)) params) locls c1 c2)
                  }
           <|> do { symbol "="
                  ; ec <- (     try (do { ePos <- getPosition
                                        ; e <- exprParserToplevel
                                        ; return $ Left e
                                        })
                            <|> do { c <- compParserToplevel
                                   ; return $ Right c
                                   }
                            <|> myfail "let <ident> = (<exp>|<comp>) in <comp>")
                  ; reserved "in"
                  ; c2 <- compParserToplevel
                  ; case ec of
                      Left e -> return (toCompPos () startPos $
                                        LetE (namePos x xPos) e c2)
                      Right c1 -> return (toCompPos () startPos $
                                          Let (namePos x xPos) c1 c2)
                  }
           <|> myfail "let <ident>[(<params>)] = (<exp>|<comp>) in <comp>"
         }

  <|> do { startPos <- getPosition
         ; reserved "lut"
         ; xPos <- getPosition
         ; x <- identifier
         ; params <- parens paramsParser
         ; symbol "="
         ; locls <- declsParser
         ; e1 <- exprParserToplevel
         ; reserved "in"
         ; c2 <- compParserToplevel           
         ; return $ toCompPos () startPos (LetFun (namePos x xPos) (toFunPos () startPos 
                              (MkFunDefined (namePos x xPos) params locls 
                              (toExpPos () startPos (ELUT Map.empty e1))
                              )) c2)
         }

  <|> do { startPos <- getPosition
         ; reserved "letexternal"
         ; xPos <- getPosition
         ; x <- identifier
         ;     do { params <- parens paramsParser
                  ; symbol ":"
                  ; ty <- parseBaseType
                  ; reserved "in"
                  ; c2 <- compParserToplevel           
                  ; return $ toCompPos () startPos (LetExternal (namePos x xPos) (toFunPos () startPos
                                    (MkFunExternal (namePos x xPos) params ty))
                                   c2)
                      
                  }
           <|> myfail "letexternal <ident>[(<params>)] = (tt in <comp>"
         }

declParser =
  do { startPos <- getPosition
     ; reserved "var"
     ;     do { xPos <- getPosition
              ; x <- identifier
              ; symbol ":"
              ; ty <- parseBaseType
              ;     do { symbol ":="
                       ; ePos <- getPosition
                       ; e <- exprParser
                       ; return $ (namePos x xPos, ty, Just $ toExpPos () ePos e)
                       }
                <|> do { return $ (namePos x xPos, ty, Nothing) }
              }
       <|> myfail "var <ident> : <base-type> [:= <exp>];"           
     }


structDefParser tn = 
  do { tfs <- sepBy (do { fn <- identifier
                        ; symbol ":" 
                        ; ft <- parseBaseType
                        ; return (fn,ft) }) (symbol ";")
     ; return (StructDef { struct_name = tn, struct_flds = tfs }) 
     }

declsParser = endBy declParser (symbol ";")

paramParser =
  do { xPos <- getPosition
     ; x <- identifier
     ; colon
     ; ty <- parseBaseType
     ; return $ (namePos x xPos, ty)
     }

paramsParser = sepBy paramParser (symbol ",")  
                          
-- parserToplevel :: Parsec String () (Prog () ())
parserToplevel =
  do { whiteSpace
     ; globs <- declsParser
     ; c <- compParserToplevel
     ; return $ MkProg globs c
     }
  
