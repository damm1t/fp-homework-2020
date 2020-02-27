module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

-- | The 's' function is S combinator.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | The 'composition' function is B combinator. Similar to '.'
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | The 'identity' function is I combinator. Similar to 'id'
identity :: a -> a
identity = s const (const (s const))

-- | The 'contraction' function is W combinator.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | The 'permutation' function is C combinator.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const composition) s) (const const)