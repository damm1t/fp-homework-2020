module Task5
  ( churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

-- | The 'zero' function returns zero in Church numerals
zero :: Nat a
zero _ x = x

-- | The 'succChurch' function is increment in Church numerals
succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)

-- | The 'succChurch' function is addition in Church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

-- | The 'succChurch' function is multiplication in Church numerals
churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

-- | The 'churchToInt' function returns integer by Church numeral
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0