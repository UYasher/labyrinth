module Classes where

class (Enum a, Bounded a, Eq a) => Cyclic a where
  next :: a -> a
  next a = if a == maxBound then minBound else succ a
  prev :: a -> a
  prev a = if a == minBound then maxBound else pred a

class Pretty a where
  pretty :: a -> String