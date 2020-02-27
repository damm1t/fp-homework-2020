{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | The 'distributivity' function expansion Either with Right pair
-- to pair of Eithers.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | The 'associator' function expansion pair with snd pair
-- to pair of fst pair.
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

-- | The 'eitherAssoc' function equals Either with Right Either
-- and Either with Left Either.
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