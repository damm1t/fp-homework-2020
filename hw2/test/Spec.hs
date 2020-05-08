module Main
  ( main
  ) where

import Test.Hspec (hspec)
import TestUtils
import TestFileDirectory

main :: IO ()
main =
  hspec $ do
    stSpec
    fdSpec