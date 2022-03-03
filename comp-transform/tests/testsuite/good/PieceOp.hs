{-# LANGUAGE ComposableTypes #-}

piececategory Expr

data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr
