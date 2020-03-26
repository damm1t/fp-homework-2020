{-# LANGUAGE LambdaCase #-}
module Task3_1 (maybeConcat) where

-- | Concat foldable structure of maybe
-- return monoid composition of elements inside (without Nothing)
maybeConcat :: (Foldable f, Monoid m) => f (Maybe m) -> m
maybeConcat = foldMap toMonad
  where
    toMonad :: (Monoid mon) => Maybe mon -> mon
    toMonad = \case
      Just v -> v
      _ -> mempty
