{-# LANGUAGE ComposableTypes #-}

module Main where

--TODO Add Common file to test when we've got module imports figured out
import Common

eval -: Expr -> Int


ext eval for Value where 
    eval (Const i) = i
   
ext eval for Op where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mult e1 e2) = eval e1 * eval e2
