{-# LANGUAGE ComposableTypes #-}

module Desug where

piececategory Expr

data piece Expr ==> Value = Const Int 
data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr 
data piece Expr ==> Sugar = Neg Expr

type Sig = Expr ==> (Value, Op)
type Sig2 = Expr ==> (Value, Op, Sugar)

desug -: for (Expr ==> a) . Expr -> a

ext for (Value in a) . desug @a for Value where
    desug c = c
    
ext for (Op in a) . desug @a for Op where
    desug (Add e1 e2) = Add (desug e1) (desug e2)
    desug (Mult e1 e2) = Mult (desug e1) (desug e2)

ext for (Value in a, Op in a) . desug @a for Sugar where
    desug (Neg e) = Const (-1) `Mult` (desug e)