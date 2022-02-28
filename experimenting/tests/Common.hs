{-# LANGUAGE ComposableTypes #-}

module Examples.Common where

-- Category declaration
piececategory Expr

-- Signature for values and operators
data piece Expr ==> Value = Const Int -- | Pair Expr Expr
data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr -- | Fst Expr | Snd Expr

-- Signature for the simple expression language
type Sig = Expr ==> (Op, Value)

eval -: Expr -> Int