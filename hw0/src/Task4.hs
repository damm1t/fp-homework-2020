module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)
-- fix :: (a -> a) -> a

-- | The 'iterateElement' function returns list of input type @a@.
iterateElement :: a -> [a]
iterateElement x = fix (x : )

-- | The 'fibonacci' function returns the Fibonacci number by number.
fibonacci :: Integer -> Integer
fibonacci = fix executorFibonacci
  where
    executorFibonacci :: (Integer -> Integer) -> Integer -> Integer
    executorFibonacci _ 0 = 1
    executorFibonacci _ 1 = 1
    executorFibonacci f x = f (x - 1) + f (x - 2)

-- | The 'factorial' function returns the factorial of number by number.
factorial :: Integer -> Integer
factorial = fix executorFactorial
  where
    executorFactorial :: (Integer -> Integer) -> Integer -> Integer
    executorFactorial _ 0 = 1
    executorFactorial _ 1 = 1
    executorFactorial f x = f (x - 1) * x

-- | The 'mapFix' function returns the factorial of number by number.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix executorMap
  where
   executorMap ::  ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
   executorMap _ _ [] = []
   executorMap f mapFunc (x : list) = mapFunc x : f mapFunc list
