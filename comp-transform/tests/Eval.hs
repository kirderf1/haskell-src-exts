{-# LANGUAGE ComposableTypes #-}

module Examples.Eval where

import Examples.Common

-- Term evaluation
eval -: Expr -> Int

eval for Op
  eval (Add x y) = x + y
  eval (Mult x y) = z * y

eval!Value (Const x) = x

-- Example: evalEx = 5
evalEx :: Int
evalEx = eval (Const 1 `Add` (Const 2 `Mult` Const 2) :: Sig)