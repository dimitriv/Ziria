-- |
-- Module      : Language.Ziria.Parser.Exceptions
-- Copyright   : (c) 2014-2015 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@cs.drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@cs.drexel.edu>

{-# LANGUAGE DeriveDataTypeable #-}

module Language.Ziria.Parser.Exceptions (
    LexerException(..),
    ParserException(..)
  ) where

import Control.Monad.Exception
import Data.Loc
import Data.Typeable (Typeable)
import Text.PrettyPrint.Mainland

data LexerException = LexerException Pos Doc
  deriving (Typeable)

instance Exception LexerException where

instance Show LexerException where
    show (LexerException pos msg) =
        pretty 80 $ nest 4 $ ppr pos <> text ":" </> msg

data ParserException = ParserException Loc Doc
  deriving (Typeable)

instance Exception ParserException where

instance Show ParserException where
    show (ParserException loc msg) =
        pretty 80 $ nest 4 $ ppr loc <> text ":" </> msg
