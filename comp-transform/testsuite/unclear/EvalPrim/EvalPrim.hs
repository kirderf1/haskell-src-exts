{-# LANGUAGE ComposableTypes #-}

module Main where

piececategory A

eval -: A -> Bool -> String

--TODO What do we do about this type signature
eval' :: (eval for A) => A -> String
eval' a = eval a False
