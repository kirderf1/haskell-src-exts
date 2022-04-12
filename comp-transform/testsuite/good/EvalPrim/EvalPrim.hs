{-# LANGUAGE ComposableTypes #-}

module EvalPrim where

piececategory A

eval -: A -> Bool -> String

eval2 :: for (eval for a) . a -> String
eval2 a = eval a False
