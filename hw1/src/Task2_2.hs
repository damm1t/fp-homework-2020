module Task2_2
  (NonEmpty(..)
  , splitOn
  , joinWith
  ) where

import Data.List (foldl')

-- | Constructor of non empty list
data NonEmpty a = a :| [a] deriving (Show)

-- | The function 'splitOn' split list @[a]@ by parameter @a@
-- return 'NonEmpty' list @[a]@
splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn element list = let  in foldr foldrArr ([] :| []) list
  where
    foldrArr c (xHead :| xTail)
          | c == element = [] :| (xHead:xTail)
          | otherwise  = (c:xHead) :| xTail

-- | The function 'joinWith' join 'NonEmpty' list @[a]@ with parameter @a@
-- return list @[a]@
joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| []) = x
joinWith element (xHead :| xTail) = foldl' joinEl xHead xTail
  where
    joinEl curHead curTail = curHead ++ (element : curTail)
