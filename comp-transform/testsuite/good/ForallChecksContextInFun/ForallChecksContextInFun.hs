{-# LANGUAGE ComposableTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module ForallChecksContextInFun where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: forall a e. for (A ==> a, B in a, c for a). Show e => a -> e -> ((), a)
d a _ = (c a, B)
