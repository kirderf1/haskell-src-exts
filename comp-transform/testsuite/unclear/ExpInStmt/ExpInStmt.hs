{-# LANGUAGE ComposableTypes #-}

module Main where

piececategory Stmt ==> Exp
piececategory Exp

data piece Stmt ==> Assign = Assign String Exp
data piece Stmt ==> If = If Exp Stmt
data piece Exp ==> Literal = BoolLit Bool | StringLit String
data piece Exp ==> BoolOp = And Exp Exp | Or Exp Exp

type AExp ==> (Literal, BoolOp)
type AStmt ==> (Assign, If) AExp

stringifyS -: Stmt -> String
stringifyE -: Exp -> String

stringifyS for Assign with stringifyE for Exp where
    stringifyS (Assign var exp) = var ++ " = " ++ stringifyE exp
stringifyS for If with stringifyE for Exp where
    stringifyS (If exp stmt) = "if (" ++ stringifyE exp ++ ") " ++ stringifyS stmt

stringifyE for Literal where
    stringifyE (BoolLit b) = show b
    stringifyE (StringLit s) = s

stringifyE for Literal where
    stringifyE (And left right) = stringifyE left ++ " && " ++ stringifyE right
    stringifyE (Or left right) = stringifyE left ++ " || " ++ stringifyE right
