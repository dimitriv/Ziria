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

module BlinkParseComp ( parseProgram, runParseM ) where

import AstExpr
import AstComp

import BlinkParseExpr

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr

import Control.Monad.Reader.Class


import Data.Maybe ( fromMaybe, fromJust )

import PpComp

{- Basic structure is the following:

comp 
  ::= comp >>> comp | comp |>>>| comp
    | comp finally comp 
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
    | return expr
    | let binding in comp

commands := sepEndBy1 ";" command  

command 
  ::= (x <- comp)
    | comp
    | let binding in command

-} 



mkPrefix parsepref cont
  = Prefix $ do { p <- getPosition
                ; pref <- parsepref
                ; return (cont p pref) 
                }

standalonePref     = reserved "standalone"
standaloneKont p _ = cStandalone (Just p) () 

repeatPref          
  = do { reserved "repeat" 
       ; p <- getPosition
       ; optionMaybe parseVectAnn 
       }

repeatKont p mb_ann = cRepeat (Just p)  () mb_ann

untilPref = do { reserved "until" 
               ; p <- getPosition
               ; parseExpr
               }

untilKont p e = cUntil (Just p) () e 


whilePref = reserved "while" >> parseExpr
whileKont p e = cWhile (Just p) () e 


timesPref 
  = choice [ do { p <- getPosition 
                ; reserved "times"
                ; e <- parseExpr
                ; let nm = mkNameFromPos (Just "_tmp_count") p (Just tint)
                ; return (eVal (Just p) () (VInt 0), e, nm)
                }
           , do { p <- getPosition
                ; reserved "for"
                ; k <- parseVarBind
                ; reserved "in"
                ; (estart,elen) <- brackets genIntervalParser
                ; return (estart,elen,k)
                }
           ] <?> "times or for"

timesKont p (estart,elen,cntnm) 
  = cTimes (Just p) () estart elen cntnm


mkPar parseop pi 
  = Infix $ do { p <- getPosition
               ; parseop
               ; return (cPar (Just p) () (mkParInfo pi)) }



parsePrimCompUnOp primname const
  = do { p <- getPosition
       ; reserved primname
       ; e <- parseExpr
       ; return (const (Just p) () e) }

parsePrimCompNullOp primname const
  = do { p <- getPosition
       ; reserved primname
       ; return (const (Just p) ()) }

parseIOComp ioname const
  = do { p <- getPosition
       ; reserved ioname
       ; mty <- optionMaybe $ brackets parseBaseType
       ; let annty = case mty of Nothing -> RWNoTyAnn
                                 Just t  -> RWRealTyAnn t
       ; return (const (Just p) () annty) }

-- Useful when parsing a command
parseBindOrComp
  = do { p <- getPosition 

       ; r <- choice [ try $ 
                       do { x <- parseVarBind 
                          ; symbol "<-"
                          ; return (Left x)
                          }
                     , do { parseComp >>= (return . Right) }
                     ] <?> "bind or computation"
       ; case r of
          Left xnm -> 
             do c <- parseComp
                return $ \mbc -> 
                  let cont = [(xnm, fromMaybe (cunit p) mbc)]
                  in cBindMany (Just p) () c cont               
          Right c -> 
             do { return $ mkoptcseq p c }
      }


-- Useful when parsing a computation
parseVarOrCall
  = do { p <- getPosition
       ; x <- identifier
       ; let xnm = mkNameFromPos (Just x) p Nothing
       ; choice [ do { notFollowedBy (symbol "(")
                     ; notFollowedBy (symbol "<-")
                     ; return (cVar (Just p) () xnm) }
                , parseCallArgsC xnm combineAsCCall
                ] <?> "variable or function call"
       }

combineAsCCall p fnm args
   = return (cCall (Just p) () fnm args) 


parseCallArgsC :: Name 
              -> (SourcePos -> Name -> 
                     [CallArg SrcExp SrcComp] -> BlinkParser a)
              -> BlinkParser a
parseCallArgsC xnm combine
  = do { p <- getPosition
       ; penv <- ask 
       ; es <- case lookup xnm penv of 
                 Just arg_info -> parens $ parse_args arg_info
                 Nothing -> parens $ 
                            sepBy (parseExpr >>= (return . CAExp )) comma
       ; combine p xnm es 
       }
  where 
    parse_arg (_,CAExp {})  = parseExpr >>= (return . CAExp)
    parse_arg (_,CAComp {}) = parseComp >>= (return . CAComp)

    parse_args []     = return []
    parse_args [p]    = parse_arg p >>= \e -> return [e]
    parse_args (p:ps) = do { e <- parse_arg p 
                           ; comma
                           ; es <- parse_args ps
                           ; return (e:es) }


parseCompBaseType
  = choice [ do { reserved "ST" 
                ; mbc <- parse_idx
                ; ti <- parseBaseType
                ; ty <- parseBaseType
                ; case mbc of 
                    Nothing -> return (TTrans ti ty)
                    Just tv -> return (TComp tv ti ty)
                }
           , parens parseCompBaseType 
           ] <?> "computation type"
  where parse_idx 
            = choice [ reserved "T" >> return Nothing
                     , do { reserved "C"
                          ; t <- parseBaseType
                          ; return (Just t) }
                     , parens parse_idx 
                     ] <?> "computation type index"


-- A vectorization annotation is just of the form [literal,literal]
parseVectAnn
  = brackets $
    do { i <- integer
       ; comma
       ; j <- integer
       ; return (fromIntegral i, fromIntegral j) 
       }



parseComp = buildExpressionParser par_op_table (choice [parse_unary_ops, parseCompTerm])
  where parse_unary_ops = choice [ from_pref standalonePref standaloneKont
                                 , from_pref repeatPref     repeatKont
                                 , from_pref untilPref      untilKont
                                 , from_pref whilePref      whileKont
                                 , from_pref timesPref      timesKont
                                 ]
        par_op_table 
           = [ [ mkPar (reservedOp ">>>")   MaybePipeline        AssocLeft ]
             , [ mkPar (reservedOp "|>>>|") (AlwaysPipeline 0 0) AssocLeft ] 
             ] 

        from_pref :: BlinkParser a -> (SourcePos -> a -> SrcComp -> SrcComp) -> BlinkParser SrcComp
        from_pref parsepref cont 
           = do { p <- getPosition
                ; pref <- parsepref
                ; c <- parseComp -- The full thing!
                ; return (cont p pref c)
                }


parseCompTerm 
  = choice [ parens parseComp
           , parsePrimCompUnOp "return" cReturn
           , parsePrimCompUnOp "emit" cEmit
           , parsePrimCompUnOp "emits" cEmits
           , parsePrimCompUnOp "takes" cTake 
           , parsePrimCompUnOp "filter" cFilter
           , parsePrimCompNullOp "take" cTake1
           , parseVarOrCall 
           , parseIOComp "read" cReadSrc
           , parseIOComp "write" cWriteSnk
 
           , do { reserved "map"
                ; p <- getPosition
                ; mb_ann <- optionMaybe parseVectAnn 
                ; e <- parseExpr
                ; return (cMap (Just p) () mb_ann e) 
                }

           , parseCompCompound
           ] <?> "computation"


type CommandCont = Maybe SrcComp -> SrcComp

parseStructDef :: BlinkParser (Either SrcComp CommandCont)
parseStructDef 
  = do { p <- getPosition
       ; reserved "struct" 
       ; updateState (\x -> x+1) 
       ; x <- identifier 
       --; debugParse p (putStrLn  "struct")
       ; symbol "="
       ; structdef <- braces (sdef_parser x)
       ; optInCont (cLetStruct (Just p) () structdef) 
       }
  where sdef_parser tn 
          = do { tfs <- sepBy parse_field semi
               ; return $ 
                 StructDef { struct_name = tn
                           , struct_flds = tfs } 
               }
        parse_field 
          = do { fn <- identifier 
               ; colon
               ; ft <- parseBaseType
               ; return (fn,ft) 
               }

parseCompCompound
  = choice 
      [ asComp parseCond "conditional used as command"
      , asComp parseStructDef "struct definition used as command"
      , asComp parseBindings  "let binding used as command"
      , do { p <- getPosition
           ; reserved "do"
           ; estmts <- parseStmtBlock <?> "statement block" 
           ; return (cReturn (Just p) () estmts) 
           }
      , optional (reserved "seq") >> braces parseCommands
      ] <?> "compound command" 

parseCommands             
  = do { cs <- sepEndBy1 parseCommand (optional semi)
       ; return $ fromJust (fold_comp cs) 
       }

  where 
          
     fold_comp [c]    = Just (c Nothing)
     fold_comp (c:cs) = Just (c (fold_comp cs))
     fold_comp []     = error "Can't happen"


parseCommand 
 = choice [ asCommand parseStructDef
          , asCommand parseCond
          , asCommand parseBindings

          , parseBindOrComp 
 
          ] <?> "command"

mkoptcseq :: SourcePos -> SrcComp -> Maybe SrcComp -> SrcComp
mkoptcseq p c1 Nothing   = c1
mkoptcseq p c1 (Just c2) = cSeq (Just p) () c1 c2


asComp :: BlinkParser (Either SrcComp CommandCont) 
       -> String 
       -> BlinkParser SrcComp
asComp m err_msg
  = do { r <- m 
       ; case r of
           Left c  -> return c
           Right k -> unexpected err_msg
       }

asCommand :: BlinkParser (Either SrcComp CommandCont) 
          -> BlinkParser CommandCont
asCommand m 
  = do { r <- m 
       ; p <- getPosition
       ; case r of 
           Left c  -> return $ mkoptcseq p c 
           Right k -> return k 
       }

parseCond :: BlinkParser (Either SrcComp CommandCont)
parseCond 
  = do { p <- getPosition
       ; reserved "if"
       ; e <- parseExpr
       ; reserved "then"
       ; c1 <- parseComp
       ; choice 
          [ do { notFollowedBy (reserved "else") 
               ; return (Right (mk_kont e c1 p)) }
          , do { reserved "else"
               ; c2 <- parseComp 
               ; return (Left (cBranch (Just p) () e c1 c2)) } 
          ]
       }
  where mk_kont e c1 p = mkoptcseq p (cBranch (Just p) () e c1 (cunit p)) 


parseBindings :: BlinkParser (Either SrcComp CommandCont)
  = do { p <- getPosition
       ; reserved "let"
       ; updateState (\x -> x + 1)  -- increment let nesting
       --; debugParse p (putStrLn  "let")
       ; choice [ do { notFollowedBy (reserved "external")
                     ; parseBinding p }
                , parseExternal p ] 
       }

parseCompOptional :: BlinkParser (Maybe (Maybe (Int,Int)))
-- Nothing -> no 'comp'
-- Just h  -> a 'comp' with an optional hint 
  = do { is_comp <- optionMaybe (reserved "comp")
       ; case is_comp of
           Nothing -> return Nothing
           Just {} -> do { r <- optionMaybe parseVectAnn
                         ; return $ Just r
                         }
       }

mkVectComp :: SrcComp -> Maybe (Int,Int) -> SrcComp
mkVectComp sc Nothing  = sc
mkVectComp sc (Just h) = cVectComp (compLoc sc) (compInfo sc) h sc

parseBinding :: SourcePos -> BlinkParser (Either SrcComp CommandCont)
parseBinding p
  = do { is_comp <- parseCompOptional
       ; x <- parseVarBind
       ; choice [ notFollowedBy (symbol "(") >> parseSimplBind is_comp x p
                , parseFunBind is_comp x p 
                ] 
       }

parseSimplBind is_comp x p 
  = do { symbol "="
       ; case is_comp of
           Nothing -> parseExpr     >>= \e -> optInCont (cLetE (Just p) () x e)
           Just h  -> parseCommands >>= \c -> optInCont (cLet  (Just p) () x (mkVectComp c h))
       }

parseFunBind is_comp x p
  = case is_comp of
      Nothing 
        -> do { params <- parens paramsParser
              ; symbol "="
              ; locls <- declsParser
              ; e <- parseStmts
              ; let fun = MkFun (MkFunDefined x params locls e) (Just p) ()
              ; optInCont (cLetFun (Just p) () x fun) 
              }
      Just h
        -> do { params <- parens compParamsParser
              ; symbol "="
              ; locls <- declsParser
              ; c <- parseCommands -- c <- parseComp
              ; extendParseEnv [(x,params)] $
                optInCont (cLetFunC (Just p) () x params locls (mkVectComp c h)) 
              }

optInCont :: (SrcComp -> SrcComp) -> BlinkParser (Either SrcComp CommandCont)
optInCont cont 
  = choice [ do { updateState (\x -> x - 1) -- still this is like popping
                ; notFollowedBy (reserved "in")
                ; p <- getPosition 
                --; debugParse p (putStrLn  "in")
                ; return (Right (\mbc -> cont (fromMaybe (cunit p) mbc))) }
           , do { updateState (\x -> x - 1) 
                ; reserved "in"             -- popping
                ; p <- getPosition
                --; debugParse p (putStrLn  "in")
                ; c <- parseComp
                ; return (Left (cont c)) }
           ]

cunit p = cReturn (Just p) () (eunit p)

parseExternal :: SourcePos -> BlinkParser (Either SrcComp CommandCont) 
parseExternal p 
  = do { reserved "external" 
       ; x <- identifier
       ; params <- parens paramsParser
       ; symbol ":"
       ; ty <- parseBaseType 
       ; let fn  = mkNameFromPos (Just x) p Nothing
       ; let fun = MkFun (MkFunExternal fn params ty) (Just p) ()
       ; optInCont (cLetExternal (Just p) () fn fun) 
       }


paramsParser = sepBy paramParser (symbol ",")  
  where paramParser =
          do { p <- getPosition
             ; x <- identifier
             ; colon
             ; ty <- parseBaseType
             ; return $ (mkNameFromPos (Just x) p (Just ty), ty)
             }

compParamsParser = sepBy paramParser (symbol ",")  
  where 
    paramParser =
       do { p <- getPosition
          ; x <- identifier
          ; colon
          ; mty <- choice [ parseBaseType     >>= ( return . CAExp )
                          , parseCompBaseType >>= ( return . CAComp )
                          ] <?> "computation parameter type"
          ; return $ (mkNameFromPos (Just x) p Nothing, mty)
          }

declsParser = endBy declParser semi
  where 
    declParser 
      = do { p <- getPosition
           ; reserved "var"
           ; x <- identifier
           ; colon 
           ; ty <- parseBaseType 
           ; let xn = mkNameFromPos (Just x) p (Just ty)
           ; mbinit <- optionMaybe (symbol ":=" >> parseExpr)
           ; return (xn, ty, mbinit) 
           }

parseProgram :: BlinkParser (Prog () ())                              
parseProgram 
  = do { whiteSpace
       ; globs <- declsParser
       ; c <- parseComp
       ; return $ MkProg globs c 
       }
