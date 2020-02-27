module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Function composition.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | Identity function.
identity :: a -> a
identity = s const (const (s const))

-- | The 'contraction' function compresses the arguments of the input
-- function and returns the changed function.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | The 'permutation' function permutes the arguments of the input
-- function and returns the changed function.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const composition) s) (const const)