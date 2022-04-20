{-# LANGUAGE ComposableTypes #-}

module PatternMatching3 where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C

d :: for B in a. a -> Int
d (B i) = i
d _     = 0
