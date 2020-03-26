module TestTreeStructure where


import Tree
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty
test1_3 :: TestTree
test1_3 = testGroup "Task 1-3 tests" [emptyTreeTest, sizeTest, findTest, addTest, delTest]

val, val3, findTree :: Tree Integer
val = Tree (0 :| []) Leaf Leaf
val3 = Tree (1 :| [1, 1]) Leaf Leaf

findTree = Tree (3 :| [])
  (Tree (2 :| [2]) (Tree (1 :| [1]) Leaf Leaf) Leaf)
  (Tree (5 :| [5])  (Tree (4 :| [4]) Leaf Leaf) (Tree (10 :| []) Leaf Leaf))

emptyTreeTest :: TestTree
emptyTreeTest =
  testGroup
    "emptyTree Tests"
    [ testCase "emptyTree Leaf" $ emptyTree Leaf @=? True
    , testCase "emptyTree not Leaf" $ emptyTree val @=? False
    ]

sizeTest :: TestTree
sizeTest =
  testGroup
    "size Tests"
    [ testCase "size Leaf" $ size Leaf @=? 0
    , testCase "size of 1 element tree" $ size val @=? 1
    , testCase "size of 3 element tree" $ size val3 @=? 3
    ]

findTest :: TestTree
findTest =
  testGroup
    "find Tests"
    [ testCase "find in Leaf" $ find Leaf (3 :: Integer) @=? Nothing
    , testCase "find 3" $ find val (3 :: Integer) @=? Nothing
    , testCase "find 5" $ find findTree 5 @=? Just (5 :| [5])
    ]


addResTree, addResTree2 :: Tree Integer
addResTree = Tree (0 :| [0]) Leaf Leaf
addResTree2 = Tree (0 :| []) Leaf (Tree (1 :| []) Leaf Leaf)

addTest :: TestTree
addTest =
  testGroup
    "add Tests"
    [ testCase "insert 0" $  show (add val 0) @=? show addResTree
    , testCase "insert 1" $  show (add val 1) @=? show addResTree2
    ]

delTest :: TestTree
delTest =
  testGroup
    "add Tests"
    [ testCase "del 0" $  show (delete addResTree 0) @=? show val
    , testCase "del 1" $  show (delete addResTree2 1) @=? show val
    , testCase "del with ins" $  show (delete (add findTree 100) 100) @=? show findTree
    ]