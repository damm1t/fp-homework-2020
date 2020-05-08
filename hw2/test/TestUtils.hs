module TestUtils
  ( stSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Utils

stSpec :: SpecWith ()
stSpec =
  describe "Utils" $ do
    it "test duplicate func" $ do
      duplicate "a" 2 `shouldBe` "aa"