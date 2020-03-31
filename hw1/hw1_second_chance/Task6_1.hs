{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Task6_1 
  ( Parser(..)
  , first
  ) where

import Control.Applicative (Alternative (..))

-- | Parser combinator constructor
newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

-- | The function 'first'
-- applies input function to first arg
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (Parser s) where
  fmap f (Parser parsingFunction) = Parser $ fmap (first f) . parsingFunction

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure result = Parser $ \input -> Just (result, input)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (Parser p1) <*> (Parser p2) = Parser action
    where
      action input =
        p1 input
          >>=
            (\(f, ret) -> fmap (first f) (p2 ret))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pLeft <|> Parser pRight =
    let split input = pLeft input <|> pRight input
    in Parser split

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser parser >>= f = Parser action
    where
      action input = parser input
       >>= \(res, ret) ->
        let (Parser modParser) = f res
        in modParser ret