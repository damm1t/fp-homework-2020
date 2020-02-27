{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherLeft, eitherRight) where
  eitherLeft :: Either a (Either b c) -> Either (Either a b) c
  eitherLeft (Left a)          = Left (Left a)
  eitherLeft (Right (Left b))  = Left (Right b)
  eitherLeft (Right (Right c)) = Right c
  
  eitherRight :: Either (Either a b) c -> Either a (Either b c)
  eitherRight (Left (Left a))  = Left a
  eitherRight (Left (Right b)) = Right (Left b)
  eitherRight (Right c)        = Right (Right c)