module Ast where

data Term =
  TmTrue
  | TmFalse
  | TmInt Integer
  | BoolLit Bool
  | Var String
  | TmApp Term Term
  | Assign String Type Term -- let foo : t = True
  | TmAbs String Type Term -- corresponds to @\x : ty. t@.
  | Pi String Type Type
  | Kind Kinds
  | TyBool
  | TyInt
  deriving(Show, Eq)

data Kinds = Star | Box deriving (Show, Eq)

type Type = Term

data Stmt =
  Seq [Stmt]
  | Function String Term [ Stmt ] -- foo (a) : T -> e
  | ExprStmt Term deriving (Show)
