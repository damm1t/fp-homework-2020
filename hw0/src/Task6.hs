module Task6
  ( expression1
  , expression2
  , foo
  , whnfExpression1
  , whnfExpression2
  ) where

import Task1 (distributivity)
import Data.Maybe (mapMaybe)

-- | First initial expression
expression1 :: (Either String b, Either String c)
expression1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Weak head normal form of 'expression1'
whnfExpression1 :: (Either String b, Either String c)
whnfExpression1 = (,)
  (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
  (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | The 'foo' function, which used in second expression
foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

-- | Second initial expression
expression2 :: Bool
expression2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

-- | Weak head normal form of 'expression2'
whnfExpression2 :: Bool
whnfExpression2 = False