{-# LANGUAGE ComposableTypes #-}

module Main where

piececategory A

eval -: A -> Bool -> String

--TODO What do we do about this type signature
eval' a = eval a False
