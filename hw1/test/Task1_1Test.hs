module Task1_1Test where

import Task1_1
import Test.Tasty
import Test.Tasty.HUnit

task1_1Test :: TestTree
task1_1Test = testGroup "Task 1-1 tests" [nextDayTest, afterDaysTest, isWeekendTest, daysToPartyTest]

nextDayTest :: TestTree
nextDayTest =
  testGroup
    "nextDay"
    [ testCase "nextDay Monday" $ assertEqual "" (nextDay Monday) Tuesday
    , testCase "nextDay Sunday" $ assertEqual "" (nextDay Sunday) Monday
    , testCase "nextDay Wednesday" $ assertEqual "" (nextDay Wednesday) Thursday
    ]

afterDaysTest :: TestTree
afterDaysTest =
  testGroup
    "afterDays"
    [ testCase "nextDay Monday equals afterDays Monday 1" $ assertEqual "" (afterDays Monday 1) Tuesday
    , testCase "nextDay Sunday equals afterDays Sunday 1" $ assertEqual "" (afterDays Sunday 1) Monday
    , testCase "Monday after 7 day" $ assertEqual "" (afterDays Monday 7) Monday
    , testCase "Friday after 2 day" $ assertEqual "" (afterDays Friday 2) Sunday
    ]

isWeekendTest :: TestTree
isWeekendTest =
  testGroup
    "isWeekend"
    [ testCase "isWeekend Monday" $ assertBool "" $ not $ isWeekend Monday
    , testCase "isWeekend Tuesday" $ assertBool "" $ not $ isWeekend Tuesday
    , testCase "isWeekend Wednesday" $ assertBool "" $ not $ isWeekend Wednesday
    , testCase "isWeekend Thursday" $ assertBool "" $ not $ isWeekend Thursday
    , testCase "isWeekend Friday" $ assertBool "" $ not $ isWeekend Friday
    , testCase "isWeekend Saturday" $ assertBool "" $ isWeekend Saturday
    , testCase "isWeekend Sunday" $ assertBool "" $ isWeekend Sunday
    ]

daysToPartyTest :: TestTree
daysToPartyTest =
  testGroup
    "daysToParty"
    [ testCase "daysToParty from Monday" $ assertEqual "" (daysToParty Monday) 4
    , testCase "daysToParty from Tuesday" $ assertEqual "" (daysToParty Tuesday) 3
    , testCase "daysToParty from Wednesday" $ assertEqual "" (daysToParty Wednesday) 2
    , testCase "daysToParty from Thursday" $ assertEqual "" (daysToParty Thursday) 1
    , testCase "daysToParty from Friday" $ assertEqual "" (daysToParty Friday) 0
    , testCase "daysToParty from Saturday" $ assertEqual "" (daysToParty Saturday) 6
    , testCase "daysToParty from Sunday" $ assertEqual "" (daysToParty Sunday) 5
    ]
