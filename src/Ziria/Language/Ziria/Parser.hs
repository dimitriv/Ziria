--------------------------------------------------------------------------------
-- |
-- Module      : Ziria.Language.Ziria.Parser
-- Copyright   : (c) 2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>
--
--------------------------------------------------------------------------------

module Ziria.Language.Ziria.Parser (
    parseProgram,
    parseProgramFromFile,
    camlParseProgram,
    camlParseProgramFromFile
  ) where

import Control.Monad.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.Loc
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Ziria.Language.Ziria.Parser.CamlParser as CP
import qualified Ziria.Language.Ziria.Parser.Parser as P
import Ziria.Language.Ziria.Parser.Monad

import Ziria.BasicTypes.AstComp

parse :: P a
      -> T.Text
      -> Pos
      -> Either SomeException a
parse p buf pos =
    evalP p (emptyPState buf pos)

parseFromFile :: P a
              -> FilePath
              -> IO a
parseFromFile p path = do
    text <- liftIO $ B.readFile path
    liftException (parse p (E.decodeUtf8 text) start)
  where
    start :: Pos
    start = startPos path

parseProgram :: T.Text
             -> Pos
             -> Either SomeException SrcProg
parseProgram = parse P.parseProgram

parseProgramFromFile :: FilePath -> IO SrcProg
parseProgramFromFile = parseFromFile P.parseProgram

camlParseProgram :: T.Text
                 -> Pos
                 -> Either SomeException SrcProg
camlParseProgram = parse CP.parseProgram

camlParseProgramFromFile :: FilePath -> IO SrcProg
camlParseProgramFromFile = parseFromFile CP.parseProgram
