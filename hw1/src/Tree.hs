{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Tree
  (Tree(..)
  , emptyTree
  , size
  , find
  , insert
  , fromList
  , toList
  , delete
  ) where

import qualified Data.List.NonEmpty as List

-- | Constructor of binary tree structure
data Tree a =
  Leaf    -- Constructor empty leaf node
  | Tree  -- Constructor non empty node
    { list :: List.NonEmpty a, left :: Tree a, right :: Tree a }
  deriving (Show)

-- | The function 'emptyTree'
-- return 'Bool' is input 'Tree' empty
emptyTree :: Tree a -> Bool
emptyTree Leaf = True
emptyTree _    = False

-- | The function 'empty'
-- return 'Bool' is input list empty
empty :: [a] -> Bool
empty [] = True
empty _ = False

-- | The function 'size'
-- return 'Int' size elements of input 'Tree'
size :: Tree a -> Int
size Leaf = 0
size tree = List.length (list tree) + size (left tree) + size (right tree)

-- | The function 'find' tries to find input node
-- return 'Nothing' if input element isn't in input 'Tree'
find :: (Ord a) => Tree a -> a -> Maybe (List.NonEmpty a)
find Leaf _ = Nothing
find Tree{..} x = case compare (List.head list) x of
  LT -> find left x
  GT -> find right x
  EQ -> Just list

-- | The function 'insert' insert input element 
-- to input 'Tree'
insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf insItem = Tree {list = insItem List.:| [], left = Leaf, right = Leaf}
insert Tree{..} insItem =
  case compare (List.head list) insItem of
    LT ->
      let newLeft = insert left insItem
       in Tree list newLeft right
    EQ -> let newList = List.cons insItem list
       in Tree newList left right
    GT ->
      let newRight = insert right insItem
       in Tree list left newRight

-- | The function 'fromList' create 'Tree' by input list
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insert Leaf

-- | The function 'fromList' create list by input 'Tree'
toList :: Tree a -> [a]
toList = foldr (:) []

-- | the function 'delete' delete input element
-- from input 'Tree' if this element contains
delete :: Ord a => Tree a -> a -> Tree a
delete Leaf _ = Leaf
delete Tree{..} delItem = case compare delItem (List.head list) of
  LT -> let newLeft = delete left delItem
    in Tree list newLeft right
  EQ -> if empty (List.tail list) then
          case extractLeftestNode right of
            Nothing                  -> left
            Just (newList, newRight) -> Tree newList left newRight
        else
          fromList (List.tail list)

  GT -> let newRight = delete right delItem
    in Tree list left newRight
  where
      extractLeftestNode :: (Ord a) => Tree a -> Maybe (List.NonEmpty a, Tree a)
      extractLeftestNode Leaf = Nothing
      extractLeftestNode Tree{list = listElements, left = leftChild, right = rightChild} =
        case extractLeftestNode leftChild of
          Nothing                    -> Just (listElements, rightChild)
          Just (childList, newLeft) -> Just (childList, Tree listElements newLeft rightChild)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f tree =
    let leftFolded = foldMap f (left tree)
        rightFolded = foldMap f (right tree)
        elemFolded = foldMap f (list tree)
     in leftFolded <> elemFolded <> rightFolded
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z tree =
    let rightFolded = foldr f z (right tree)
        elemFolded = foldr f rightFolded (list tree)
     in foldr f elemFolded (left tree)
