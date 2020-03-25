{-# LANGUAGE LambdaCase #-}
module Task1_2
  (Nat(..)
  , fromNumberToNat
  , fromNatToNumber
  , isEven
  , divn
  , modn
  ) where

data Nat = Z | S Nat deriving (Show)

instance Num Nat where
  (+) x Z     = x
  (+) x (S y) = S (x + y)

  (*) _ Z     = Z
  (*) x (S y) = x + (x * y)

  (-) x Z         = x
  (-) Z _         = Z
  (-) (S x) (S y) = x - y

  abs = id
  signum Z = Z
  signum _ = S Z
  fromInteger 0 = Z
  fromInteger x = S (fromInteger (x - 1))

fromNumberToNat :: (Ord t, Num t) => t -> Nat
fromNumberToNat x = if x > 0 then S (fromNumberToNat (x - 1)) else Z

fromNatToNumber :: (Num t) => Nat -> t
fromNatToNumber (S x) = fromNatToNumber x + 1
fromNatToNumber Z = 0

instance Eq Nat where
  Z == Z = True
  (S x) == (S y) = x == y
  _ == _ = False

instance Ord Nat where
  compare x y = compare (fromNatToNumber x :: Integer) (fromNatToNumber y :: Integer)

isEven :: Nat -> Bool
isEven = \case
  Z -> True
  S Z -> False
  S (S x) -> isEven x


divn :: Nat -> Nat -> Nat
divn _ Z = error "Division by Zero"
divn x y
  | x == y = S Z
  | x < y = Z
  | otherwise = divn (x - y) y + 1
  
modn :: Nat -> Nat -> Nat
modn _ Z = error "Division by Zero"
modn x y
  | x == y = Z
  | x < y = x
  | otherwise = modn (x - y) y