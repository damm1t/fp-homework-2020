{-# LANGUAGE LambdaCase #-}
module Task1_1
  ( Day(..)
  ,nextDay
  , afterDays
  , isWeekend
  , daysToParty
  )  where

-- | Structure day of week
data Day = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq Day where
  Monday == Monday       = True
  Tuesday == Tuesday     = True
  Wednesday == Wednesday = True
  Thursday == Thursday   = True
  Friday == Friday       = True
  Saturday == Saturday   = True
  Sunday == Sunday       = True
  _ == _                 = False

instance Enum Day where
  fromEnum = \case
    Monday    -> 0
    Tuesday   -> 1
    Wednesday -> 2
    Thursday  -> 3
    Friday    -> 4
    Saturday  -> 5
    Sunday    -> 6

  toEnum x = case x `mod` 7 of
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    6 -> Sunday
    _ -> error "Incorrect toEnum"

-- | The function 'nextDay' 
-- returns 'Day' after input 'Day'
nextDay :: Day -> Day
nextDay = succ

-- | The function 'afterDays'
-- returns 'Day' after input period 
-- which starts with input 'Day'
afterDays :: Day -> Int -> Day
afterDays day count =
  let countDiff = (count + 7) `mod` 7
  in toEnum ((fromEnum day + countDiff) `mod` 7)

-- | The function 'isWeekend'
-- returns 'Bool' is the input 'Day' weekend
isWeekend :: Day -> Bool
isWeekend = \case
  Saturday -> True
  Sunday   -> True
  _        -> False

-- | The function 'daysToParty'
-- returns 'Int' count of days to 'Friday'
-- by input 'Day'
daysToParty :: Day -> Int
daysToParty day = (4 + 7 - fromEnum day) `mod` 7