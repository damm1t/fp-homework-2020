{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}

module Task2_2
  (splitOn
  , joinWith
  ) where

import Data.List (foldl')

data NonEmpty a = a :| [a] deriving (Show)

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn element list = foldr folder ([] :| []) list
  where
    folder c (x :| xs)
      | c == element = [] :| (x:xs)
      | otherwise  = (c:x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| []) = x
joinWith element (x :| xs) = foldl' (\a b -> a ++ (element:b)) x xs
