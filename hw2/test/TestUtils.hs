module TestUtils
  ( stSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import qualified Data.ByteString.Char8 as BS
import Utils

stSpec :: SpecWith ()
stSpec =
  describe "Utils" $ do
    it "test duplicate func" $ do
      duplicate "a" 2 `shouldBe` "aa"
      duplicate "->" 2 `shouldBe` "->->"
    it "test addStrToBS func" $ do
      addStrToBS "c" (BS.pack "a") `shouldBe` BS.pack "a\nc" 
      addStrToBS "" BS.empty `shouldBe` BS.pack "\n"