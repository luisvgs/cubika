module Main where

import           Ast             (Kinds (..), Term (..), Type (..))
import           Control.Monad   (unless)
import           Cube
import           Data.Either
import           Data.Void       (Void)
import           Env
import           Parser
import           System.IO
import           Text.Megaparsec

main :: IO ()
main = repl

repl :: IO ()
repl = do
    input <- read'
    unless (input == ":q") $ do
        case run exprParser input of
            Left err   -> putStrLn $ "Parse error: " ++ errorBundlePretty err
            Right expr -> print' $ show expr ++ "\n"

-- foo = run exprParser "let id (A : Type) (x : A) : A = x in let Bool : Type; False : Bool in id Bool False"
-- foo = run exprParser "let Bool : Type; False : Bool; id (A : Type) (x : A) : A = x in id Bool False"
-- foo = run exprParser "idt Bool False;"
foo = run exprParser "let x : Type = match x with | 1 -> true | _ -> false in x"
-- foo = run exprParser "let id : forall (a : Type) . a -> a; id a x"
-- foo = run exprParser "let x : Type = g a; y : Type = f x in h x y"
-- foo = run exprParser "let Bool : Type;Bool=forall (a : Type) . Bool -> a -> a; False : Bool in False"
foo' = case foo of
    Right (expr) -> expr
    Left x       -> error "X"

foo'' = do
  t <- typeof initialEnv foo'
  let (e, t') = t
  return t'

read' :: IO String
read' = putStr ">> " >> hFlush stdout >> getLine

print' :: String -> IO ()
print' = putStrLn
