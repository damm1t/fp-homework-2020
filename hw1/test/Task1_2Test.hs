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

zero, one, two, three, five, six :: Nat
zero = Z
one = S zero
two = S one
three = S two


addNatTest :: TestTree
addNatTest =
  testGroup
    "addNat Tests"
    [ testCase "0 + 0 = 0" $ (zero + zero) @=? zero
    , testCase "0 + 1 = 1" $ (zero + one) @=? one
    , testCase "1 + 0 = 0 + 1 = 1" $ (zero + one) @=? (one + zero)
    , testCase "1 + 2 = 3" $ (one + two) @=? three
    ]

five = two + three
six = three + three

mulNatTest :: TestTree
mulNatTest =
  testGroup
    "mulNat Tests"
    [ testCase "0 * 1 = 0" $ (zero + zero) @=? zero
    , testCase "2 * 1 = 2" $ (two * one) @=? two
    , testCase "1 * 2 = 2 * 1" $ (one * two) @=? (two * one)
    , testCase "3 * 2 = 6" $ (three * two) @=? six
    ]

subNatTest :: TestTree
subNatTest =
  testGroup
    "subNat Tests"
    [ testCase "0 - 0 = 0" $ (Z - Z) @=? Z
    , testCase "0 - 1 = 0" $ (Z - one) @=? Z
    , testCase "1 - 0 = 1" $ (one - Z) @=? one
    , testCase "2 - 1 = 1" $ (two - one) @=? one
    ]

fromNatToNumberTest :: TestTree
fromNatToNumberTest =
  testGroup
    "fromNatToNumber Tests"
    [ testCase "fromNatToNumber Z" $ (fromNatToNumber (Z :: Nat) :: Integer) @=? 0
    , testCase "fromNatToNumber (S Z)" $ (fromNatToNumber (S Z :: Nat) :: Integer) @=? 1
    , testCase "fromNatToNumber (S (S Z))" $ (fromNatToNumber (S (S Z) :: Nat) :: Integer) @=? 2
    ]

fromNumberToNatTest :: TestTree
fromNumberToNatTest =
  testGroup
    "fromNumberToNat Tests"
    [ testCase "fromNumberToNat 0" $ fromNumberToNat (0 :: Integer) @=? Z
    , testCase "fromNumberToNat 5" $
      fromNumberToNat (5 :: Integer) @=? five
    , testCase "fromNumberToNat 228" $
      fromNatToNumber (fromNumberToNat (228 :: Integer)) @=? (228 :: Integer)
    ]

eqNatTest :: TestTree
eqNatTest =
  testGroup
    "Eq Nat Tests"
    [ testCase "Equals Z Z" $ assertBool "" (Z == Z)
    , testCase "Equals (S Z) (S Z)" $  assertBool "" (one == one)
    , testCase "Not Equals (S Z) Z" $ assertBool "" (one /= Z)
    ]

ordNatTest :: TestTree
ordNatTest =
  testGroup
    "Ord Nat Tests"
    [ testCase "(S Z) > Z" $ assertBool "" (S Z > Z)
    , testCase "(S Z) < (S (S Z))" $  assertBool "" (S Z < two)
    , testCase "Z >= Z" $ assertBool "" (Z >= Z)
    , testCase "(S Z) <= (S Z)" $ assertBool "" (S Z <= S Z)
    ]

isEvenTest :: TestTree
isEvenTest =
  testGroup
    "isEven Nat Tests"
    [ testCase "is even (S Z)" $ assertBool "" (not (isEven one))
    , testCase "Not is even (S (S Z))" $  assertBool "" (isEven two)
    , testCase "is even Z" $ assertBool "" (isEven Z)
    , testCase "is even (228 :: Nat)" $ assertBool "" (isEven (fromNumberToNat (228 :: Integer)))
    ]

divNatTest :: TestTree
divNatTest =
  testGroup
    "div Nat Tests"
    [ testCase "5 div 3" $ divn (fromNumberToNat (5 :: Integer)) (fromNumberToNat (3 :: Integer)) @=? 1
    , testCase "228 div 20" $ divn (fromNumberToNat (228 :: Integer)) (fromNumberToNat (20 :: Integer)) @=? 11
    , testCase "20 div 228" $ divn (fromNumberToNat (20 :: Integer)) (fromNumberToNat (228 :: Integer)) @=? 0
    ]

modNatTest :: TestTree
modNatTest =
  testGroup
    "mod Nat Tests"
    [ testCase "6 mod 3" $ modn (fromNumberToNat (6 :: Integer)) (fromNumberToNat (3 :: Integer)) @=? 0
      , testCase "228 mod 20" $ modn (fromNumberToNat (228 :: Integer)) (fromNumberToNat (20 :: Integer)) @=? 8
      , testCase "20 mod 228" $ modn (fromNumberToNat (20 :: Integer)) (fromNumberToNat (228 :: Integer)) @=? 20
      ]