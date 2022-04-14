{-# LANGUAGE ComposableTypes #-}

module ChecksContextInFun where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: for (A ==> a, B in a, c for a). Show e => a -> e -> ((), a)
d a _ = (c a, B)
