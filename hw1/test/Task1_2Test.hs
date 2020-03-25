module Task1_2Test where

import Task1_2
import Test.Tasty
import Test.Tasty.HUnit

task1_2Test :: TestTree
task1_2Test = testGroup "Task 1-2 tests" [
  addNatTest
  , mulNatTest
  , subNatTest
  , fromNatToNumberTest
  , fromNumberToNatTest
  , eqNatTest
  , ordNatTest
  , isEvenTest
  , divNatTest
  , modNatTest
  ]

addNatTest :: TestTree
addNatTest =
  testGroup
    "addNat"
    [ testCase "addNat Z Z" $ (Z + Z) @=? Z
    , testCase "addNat Z (S Z)" $ (Z + S Z) @=? S Z
    , testCase "addNat (S Z) Z" $ (Z + S Z) @=? (S Z + Z)
    , testCase "addNat (S Z) (S (S Z))" $ (S Z + S (S Z)) @=? S (S $ S Z)
    ]

mulNatTest :: TestTree
mulNatTest =
  testGroup
    "mulNat"
    [ testCase "mulNat Z (S Z)" $ (Z + Z) @=? Z
    , testCase "mulNat (S (S Z)) (S Z)" $ (S (S Z) * S Z) @=? S (S Z)
    , testCase "mulNat (S Z) (S (S Z))" $ (S Z * S (S Z)) @=? (S (S Z) * S Z)
    , testCase "mulNat (S (S (S Z))) (S (S Z))" $ (S (S (S Z)) * S (S Z)) @=? S (S $ S $ S $ S $ S Z)
    ]

subNatTest :: TestTree
subNatTest =
  testGroup
    "subNat"
    [ testCase "subNat Z Z" $ (Z - Z) @=? Z
    , testCase "subNat Z (S Z)" $ (Z - S Z) @=? Z
    , testCase "subNat (S Z) Z" $ (S Z - Z) @=? S Z
    , testCase "subNat (S (S Z)) (S Z)" $ (S (S Z) - S Z) @=? S Z
    ]

fromNatToNumberTest :: TestTree
fromNatToNumberTest =
  testGroup
    "fromNatToNumber"
    [ testCase "fromNatToNumber Z" $ (fromNatToNumber (Z :: Nat) :: Integer) @=? 0
    , testCase "fromNatToNumber (S Z)" $ (fromNatToNumber (S Z :: Nat) :: Integer) @=? 1
    , testCase "fromNatToNumber (S (S Z))" $ (fromNatToNumber (S (S Z) :: Nat) :: Integer) @=? 2
    ]

fromNumberToNatTest :: TestTree
fromNumberToNatTest =
  testGroup
    "fromNumberToNat"
    [ testCase "fromNumberToNat 0" $ fromNumberToNat (0 :: Integer) @=? Z
    , testCase "fromNumberToNat 5" $
      fromNumberToNat (5 :: Integer) @=? S (S $ S $ S $ S Z)
    , testCase "fromNumberToNat 228" $
      fromNatToNumber (fromNumberToNat (228 :: Integer)) @=? (228 :: Integer)
    ]

eqNatTest :: TestTree
eqNatTest =
  testGroup
    "Eq Nat"
    [ testCase "Equals Z Z" $ assertBool "" (Z == Z)
    , testCase "Equals (S Z) (S Z)" $  assertBool "" (S Z == S Z)
    , testCase "Not Equals (S Z) Z" $ assertBool "" (S Z /= Z)
    ]

ordNatTest :: TestTree
ordNatTest =
  testGroup
    "Ord Nat"
    [ testCase "(S Z) > Z" $ assertBool "" (S Z > Z)
    , testCase "(S Z) < (S (S Z))" $  assertBool "" (S Z < S (S Z))
    , testCase "Z >= Z" $ assertBool "" (Z >= Z)
    , testCase "(S Z) <= (S Z)" $ assertBool "" (S Z <= S Z)
    ]

isEvenTest :: TestTree
isEvenTest =
  testGroup
    "isEven Nat"
    [ testCase "is even (S Z)" $ assertBool "" (not (isEven (S Z)))
    , testCase "Not is even (S (S Z))" $  assertBool "" (isEven (S $ S Z))
    , testCase "is even Z" $ assertBool "" (isEven Z)
    , testCase "is even (228 :: Nat)" $ assertBool "" (isEven (fromNumberToNat (228 :: Integer)))
    ]

divNatTest :: TestTree
divNatTest =
  testGroup
    "div Nat"
    [ testCase "5 div 3" $ divn (fromNumberToNat (5 :: Integer)) (fromNumberToNat (3 :: Integer)) @=? 1
    -- testCase "div by 0" (divn (fromNumberToNat (5 :: Integer)) (fromNumberToNat (0 :: Integer))) @=? error
    , testCase "228 div 20" $ divn (fromNumberToNat (228 :: Integer)) (fromNumberToNat (20 :: Integer)) @=? 11
    , testCase "20 div 228" $ divn (fromNumberToNat (20 :: Integer)) (fromNumberToNat (228 :: Integer)) @=? 0
    ]

modNatTest :: TestTree
modNatTest =
  testGroup
    "mod Nat"
    [ testCase "6 mod 3" $ modn (fromNumberToNat (6 :: Integer)) (fromNumberToNat (3 :: Integer)) @=? 0
      -- testCase "div by 0" (divn (fromNumberToNat (5 :: Integer)) (fromNumberToNat (0 :: Integer))) @=? error
      , testCase "228 mod 20" $ modn (fromNumberToNat (228 :: Integer)) (fromNumberToNat (20 :: Integer)) @=? 8
      , testCase "20 mod 228" $ modn (fromNumberToNat (20 :: Integer)) (fromNumberToNat (228 :: Integer)) @=? 20
      ]