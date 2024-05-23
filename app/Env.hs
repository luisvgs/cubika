module Env where

import           Ast      (Kinds (..), Term (..), Type (..))
import           Cube     (Context)
import qualified Data.Map as Map

type Environment = Map.Map String Type



-- Predefined terms
-- Pi \ a : * \ x : a . x
idt = 
-- idt = TmAbs "a" (Kind Star) $ TmAbs "x" (Var "a") (Var "x")
-- \x : Type . x : Type
id' = TmAbs "x" (Kind Star) $ Var "x"
-- \f : Nat -> Nat . \z : Nat. z
zero = TmAbs "f" (TmAbs "a" (Kind Star) $ Var "a") $ TmAbs "z" (Var "Nat") $ Var "z"
-- \ f : Nat . \z . f z
-- succ :: Term
-- succ = TmAbs "f" (TmVar "Nat") $ TmAbs "x" ()

-- Πa:*.Πx:a.a
-- id' = TmAbs "a" (Kind Star) $ TmAbs "x" (Var "a") (Var "x")
-- idt = Pi "a" (Kind Star) $ Pi "x" (Var "a") (Var "a")

emptyEnv :: Environment
emptyEnv = Map.empty

extend :: Environment -> String -> Term -> Environment
extend env v t = Map.insert v t env
-- (x : bool . x) true
initialEnv :: Context
initialEnv =
    Map.fromList
        [
         ("Bool", Kind Star),
         ("True", Var "Bool"),
         ("False", Var "Bool"),
         -- ("id", id'),
         ("idt", idt)
        -- , ("B", Pi "x" (Var "A") (Kind Star))
        ]












