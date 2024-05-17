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
    | TmSeq [Term]
    | ExprStmt Term
    | Pi String Type Type
    | Kind Kinds
    | -- | Universe String Type
      TyBool
    | TyInt
    deriving (Show, Eq)

data Kinds = Star | Box deriving (Show, Eq)

type Type = Term
