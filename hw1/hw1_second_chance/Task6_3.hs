{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Task6_3
  ( BracketSeq(..)
  , Sign
  , bracketParser
  , numberParser
  ) where

import Task6_1
import Task6_2
import Control.Applicative ((<|>))
import Data.Char (isDigit)

-- | Correct Bracket Sequence constructor
data BracketSeq =
  -- | node represents empty sequence
  Empty 
  -- | node represents inner of sequence
  | Inner BracketSeq
  -- | node represents concatenation of sequences
  | Concat BracketSeq BracketSeq

instance Show BracketSeq where
  show Empty               = ""
  show (Inner inner)       = "(" ++ show inner ++ ")"
  show (Concat a b) = show a ++ show b

-- | Bracket Parser implementation
bracketParser :: Parser Char BracketSeq
bracketParser = bracketsToParse <* eof
  where
    bracketsToParse :: Parser Char BracketSeq
    bracketsToParse = let emptyBrackets = Empty <$ ok
      in concatParser <|> emptyBrackets

    concatParser :: Parser Char BracketSeq
    concatParser = Concat <$> innerParser <*> bracketsToParse

    innerParser :: Parser Char BracketSeq
    innerParser = Inner <$> (element '(' *> bracketsToParse <* element ')')

-- | Constructor the sign of number 
data Sign =
  -- | '+'
  Plus
  -- | '-'
  | Minus

-- | number Parser implementation
numberParser :: forall t. Num t => Parser Char t
numberParser = unsignedNumParser <|> signedNumParser
  where
    signedNumParser :: Parser Char t
    signedNumParser = (Minus <$ element '-' <|> Plus <$ element '+') >>=
      (\sign -> unsignedNumParser >>=
      (\num -> return $
        case sign of
          Plus  -> num
          Minus -> (-1) * num)
      )

    unsignedNumParser :: Parser Char t
    unsignedNumParser = fmap fst numParser

    numParser :: Parser Char (t, t)
    numParser = satisfy isDigit >>=
      (\cur -> numParser <|> parseZero >>=
        (\(curNum, curPow) ->
          return (mapToNum cur * curPow + curNum, 10 * curPow)))

    parseZero :: Parser Char (t, t)
    parseZero = (0, 1) <$ ok

    mapToNum :: Char -> t
    mapToNum = \case
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      _   -> 9