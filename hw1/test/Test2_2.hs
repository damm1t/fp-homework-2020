module Test2_2 where

import Task2_2
import Test.Tasty
import Test.Tasty.HUnit

test2_2 :: TestTree
test2_2 = testGroup "Task 2-2 tests" [splitOnTest, joinWithTest, idTest]

val1, val2, val3 :: NonEmpty String
val1 = "path" :| ["to","file"]
val2 = "abracadabra" :| []
val3 =  "" :| []

splitOnTest :: TestTree
splitOnTest =
  testGroup
    "splitOn Tests"
    [ testCase "splitOn '/' \"path/to/file\"" $
      show (splitOn '/' "path/to/file") @=? show val1
    , testCase "splitOn '/' \"abracadabra\"" $
      show (splitOn '/' "abracadabra") @=? show val2
    , testCase "splitOn '/' \"\"" $ show (splitOn '/' "") @=? show val3
    ]


joinWithTest :: TestTree
joinWithTest =
  testGroup
    "joinWith Tests"
    [ testCase "joinWith '/' (\"path\" :| [\"to\", \"file\"])" $
      joinWith '/' ("path" :| ["to", "file"]) @=? "path/to/file"
    , testCase "joinWith '/' (\"abracadabra\" :| [])" $
      joinWith '/' ("abracadabra" :| []) @=? "abracadabra"
    , testCase "joinWith ''/' (\"\" :| [])" $ joinWith '/' ("" :| []) @=? ""
    ]
idTest :: TestTree
idTest =
  testGroup
    "joinWith x . splitOn x ≡ id"
    [ testCase "x = \"path/to/file\"" $
      show (joinWith '/' (splitOn '/' "path/to/file")) @=? "\"path/to/file\""
    , testCase "x = \"abracadabra\"" $
      show (joinWith '/' (splitOn '/' "abracadabra")) @=? "\"abracadabra\""
    , testCase "x = \"\"" $
          show (joinWith '/' (splitOn '/' "")) @=? "\"\""
    ]
