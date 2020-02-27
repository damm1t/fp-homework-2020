module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | The 'doubleNeg' function inhabits type @a -> Neg (Neg a)@.
doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

-- | The 'excludedNeg' function inhabits type @Neg (Neg (Either a (Neg a)))@.
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- unprovable in intuitionistic logic.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- unprovable in intuitionistic logic.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | The 'thirdNegElim' function inhabits type @Neg (Neg (Neg a)) -> Neg a@.
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f = f . doubleNeg