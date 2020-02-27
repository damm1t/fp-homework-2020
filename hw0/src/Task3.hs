module Task3
  ( composition
  , contraction
  , identity
  , s
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

identity :: a -> a
identity = s const (const (s const))

contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)