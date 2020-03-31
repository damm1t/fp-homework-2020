{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task6_2
  ( element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Task6_1

ok :: Parser s ()
ok = return ()

eof :: Parser s ()
eof = Parser isEmptyStream
  where
    isEmptyStream :: [s] -> Maybe ((), [s])
    isEmptyStream [] = Just ((), [])
    isEmptyStream _  = Nothing

satisfy :: forall s . (s -> Bool) -> Parser s s
satisfy predicate = Parser symbolChecker
  where
    symbolChecker :: [s] -> Maybe (s, [s])
    symbolChecker [] = Nothing
    symbolChecker (x:xs)
      | predicate x = Just (x, xs)
      | otherwise   = Nothing

element :: (Eq s) => s -> Parser s s
element x = satisfy (== x)

stream :: forall s . (Eq s) => [s] -> Parser s [s]
stream list = Parser $ fmap (list, ) . prefixGetter list
  where
    prefixGetter :: [s] -> [s] -> Maybe [s]
    prefixGetter (_:_) []        = Nothing
    prefixGetter [] [] = Just []
    prefixGetter [] (y:ys) = Just (y:ys)
    prefixGetter (x:xs) (y:ys)
      | x == y    = prefixGetter xs ys
      | otherwise = Nothing