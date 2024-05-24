module Ast where

data Term
    = TmTrue
    | TmFalse
    | TmInt Integer
    | BoolLit Bool
    | Var String
    | TmApp Term Term
    | Let String Type Term Term -- let Nat : * = term
    | TmAbs String Type Term -- corresponds to @\x : ty. t@.
    | Function String Term Type Term
    | TmMatch Term [(Pattern, Term)]
    | Pi String Type Type
    | Kind Kinds
    | TyBool
    | TyInt
    deriving (Show, Eq)

data Pattern = PInt Int | PBool Bool | Wildcard deriving (Show, Eq)

data Kinds = Star | Box  | TrustMe deriving (Show, Eq)

type Type = Term

-- instance Show Term where
--   show TmTrue = "true"
--   show TmFalse = "false"
--   show TyInt = "Int"
--   show TyBool = "Bool"
--   show (TmInt i) = show i
--   show (BoolLit b) = show b
--   show (Var x) = x
--   show (TmApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
--   show (Let x ty t1 t2) = "let " ++ x ++ " : " ++ show ty ++ " = " ++ show t1 ++ " in " ++ show t2
--   show (TmAbs x ty t) = "(\\" ++ x ++ " : " ++ show ty ++ ". " ++ show t ++ ")"
--   show (Function x t1 ty t2) = "fun " ++ x ++ " : " ++ show ty ++ " => " ++ show t1 ++ " : " ++ show t2
--   show (Pi x ty1 ty2) = "(" ++ x ++ " : " ++ show ty1 ++ ") -> " ++ show ty2
--   show (Kind k) = show k

-- instance Show Kinds where
--   show Star = "Type"
--   show Box  = "box"
