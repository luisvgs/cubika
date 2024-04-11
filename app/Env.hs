module Env where
import           Ast                            ( Kinds(..)
                                                , Term(..)
                                                , Type(..)
                                                )
import           Cube                           ( Context )
import qualified Data.Map                      as Map

type Environment = Map.Map String Type

emptyEnv :: Environment
emptyEnv = Map.empty

extend :: Environment -> String -> Term -> Environment
extend env v t = Map.insert v t env

initialEnv :: Context
initialEnv = Map.fromList
  [ ("foo", BoolLit True)
  , ("Nat", Kind Star)
  , ("A"  , Kind Star)
  , ("B"  , Pi "x" (Var "A") (Kind Star))
  ]
