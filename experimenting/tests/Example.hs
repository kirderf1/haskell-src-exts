
{-# LANGUAGE ComposableTypes #-}

data piece Expr ==> Op = Add Expr Expr
    deriving Show

-- $(derive [makeTraversable, makeFoldable,
--           makeEqF, makeShowF, smartConstructors, smartAConstructors]
--          [''Value, ''Op])
