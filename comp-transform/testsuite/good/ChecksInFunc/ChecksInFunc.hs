{-# LANGUAGE ComposableTypes #-}

module ChecksInFunc where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: for (A ==> a, B in a, c for a). a -> ((), a)
d a = (c a, B)
