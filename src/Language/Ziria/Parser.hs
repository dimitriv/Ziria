--------------------------------------------------------------------------------
-- |
-- Module      : Language.Ziria.Parser
-- Copyright   : (c) 2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>
--
--------------------------------------------------------------------------------

module Language.Ziria.Parser (
    parseProgram,
    parseProgramFromFile
  ) where

import Control.Monad.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.Loc
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Language.Ziria.Parser.Parser as P
import Language.Ziria.Parser.Monad

import AstComp

parse :: P a
      -> T.Text
      -> Pos
      -> Either SomeException a
parse p buf pos =
    evalP p (emptyPState buf pos)

parseProgram :: T.Text
             -> Pos
             -> Either SomeException SrcProg
parseProgram = parse P.parseProgram

parseProgramFromFile :: FilePath -> IO SrcProg
parseProgramFromFile path = do
    text <- liftIO $ B.readFile path
    liftException (parseProgram (E.decodeUtf8 text) start)
  where
    start :: Pos
    start = startPos path
