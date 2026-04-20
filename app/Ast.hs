module Ast where

import Data.Data

data Name = Name {
    string :: String,
    index :: Int
} deriving (Data, Eq, Ord)

instance Show Name where
    show (Name s 0) = s
    show (Name s i) = s ++ show i

data BuiltinValue = BinOp BinOpType | PartialBinOp BinOpType Value deriving (Data, Eq)

instance Show BuiltinValue where
    show (BinOp op) = show op
    show (PartialBinOp op v) = "(" ++ show op ++ " " ++ show v ++ ")"

data BinOpType = Add | Sub | Mul | Eq | And | Or deriving (Data, Eq)

instance Show BinOpType where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Eq = "=="
    show And = "&"
    show Or = "|"

data Value = Var Name | Abs Name Expr | VInt Int | VBool Bool | Builtin BuiltinValue deriving (Data, Eq)

instance Show Value where
    show (Var nm) = show nm
    show (Abs nm e) = "(\\" ++ show nm ++ ". " ++ show e ++ ")"
    show (VInt n) = show n
    show (VBool b) = show b
    show (Builtin t) = show t

data Expr = Val Value
    | App Expr Expr
    | Let Name Expr Expr
    | IfElse Expr Expr Expr
    | Shift Name Expr
    | Reset Expr
    deriving (Data, Eq)

instance Show Expr where
    show (Val v) = show v
    show (App e1 (App e21 e22)) = show e1 ++ " (" ++ show (App e21 e22) ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2
    show (Let nm e1 e2) = "let " ++ show nm ++ " = " ++ show e1 ++ " in " ++ show e2
    show (IfElse e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Shift nm e) = "(shift " ++ show nm ++ ". " ++ show e ++ ")"
    show (Reset e) = "reset (" ++ show e ++ ")"
