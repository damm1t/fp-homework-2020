module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- недоказуемо в иив
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- недоказуемо в иив
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f = f . doubleNeg