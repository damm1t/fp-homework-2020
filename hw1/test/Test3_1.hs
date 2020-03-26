module Test3_1 where


import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, describe, it, shouldBe, testSpec)

import Task3_1

maybeConcatTree :: IO TestTree
maybeConcatTree = testSpec "Tests 3-1" maybeConcatSpec

maybeConcatSpec :: Spec
maybeConcatSpec = describe "maybeConcat Tests" $ do
  it "maybeConcat [] :: [Maybe String]" $  maybeConcat ([] :: [Maybe String]) `shouldBe` ""
  it "maybeConcat [Nothing, Nothing] :: [Maybe [Int]]" $  maybeConcat ([Nothing, Nothing] :: [Maybe [Int]]) `shouldBe` []
  it "maybeConcat [Just [1, 2], Nothing, Just [4, 5]] :: [Maybe [Int]]" $  maybeConcat ([Just [1, 2], Nothing, Just [4, 5]] :: [Maybe [Int]]) `shouldBe` [1, 2, 4, 5]
