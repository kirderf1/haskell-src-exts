{-# LANGUAGE ComposableTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module ForallChecksContextInFun where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: forall a e. (A ==> a, B with a, a with c, Show e) => a -> e -> ((), a)
d a _ = (c a, B)
