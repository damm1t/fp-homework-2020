module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)
-- fix :: (a -> a) -> a

iterateElement :: a -> [a]
iterateElement x = fix (x:)

fibonacci :: Integer -> Integer
fibonacci = fix executor
  where
    executor :: (Integer -> Integer) -> Integer -> Integer
    executor _ 0 = 1
    executor _ 1 = 1
    executor f x = f (x - 1) + f (x - 2)


factorial :: Integer -> Integer
factorial = fix executor
  where
    executor :: (Integer -> Integer) -> Integer -> Integer
    executor _ 0 = 1
    executor _ 1 = 1
    executor f x = f (x - 1) * x

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix executor
  where
   executor ::  ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
   executor _ _ [] = []
   executor f mapFunc (x:list) = mapFunc x:f mapFunc list
