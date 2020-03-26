module TestError where

import Control.Exception (evaluate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, describe, it, shouldThrow,
  testSpec)

import Task1_2

natTestTree :: IO TestTree
natTestTree = testSpec "Tests with error" natSpec

natSpec :: Spec
natSpec = describe "Division by Zero" $ do 
  it "5 div 0" $ evaluate (five `divn` zero) `shouldThrow` anyErrorCall
  it "5 mod 0" $ evaluate (five `modn` zero) `shouldThrow` anyErrorCall

  where
    zero = Z
    five = fromNumberToNat (5 :: Integer)