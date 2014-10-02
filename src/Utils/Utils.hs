{-# OPTIONS_GHC -Wall #-}
-- | Generic utils
module Utils (
    panic
  ) where

import Text.PrettyPrint.HughesPJ
import System.IO.Unsafe (unsafePerformIO)
import System.Exit

panic :: Doc -> a
panic err = unsafePerformIO $ do
  print $ text "Panic! The impossible happened!" $$ err
  exitFailure
