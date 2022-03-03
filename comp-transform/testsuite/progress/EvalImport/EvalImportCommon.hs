{-# LANGUAGE ComposableTypes #-}

module Main where

import Common

eval -: Expr -> Int


eval for Value where 
    eval (Const i) = i
   
eval for Op where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mult e1 e2) = eval e1 * eval e2
