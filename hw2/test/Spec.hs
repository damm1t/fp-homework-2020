module Main
  ( main
  ) where

import Test.Hspec (hspec)
import TestUtils

main :: IO ()
main =
  hspec $ do
    stSpec
