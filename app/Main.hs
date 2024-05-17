module Main where

import           Ast             (Term (..), Type (..))
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
    -- input <- read'
    -- unless (input == ":q") $ do
    case run exprParser "let x :: Int = g a ;y :: Int = f x in h" of
        Left err   -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        -- Right expr -> print' $ show expr ++ "\n"
        Right expr -> print' $ show expr ++ "\n"
-- parseExpr :: String -> Expr
-- parseExpr input =
--   case parse (contents expr) "<stdin>" input of
--     Left err -> error (show err)
--     Right ast -> ast

-- main :: IO ()
-- main = getLine >>= print . parseExpr >> main

-- repl

-- Translate the following:
-- --Πa:*.Πx:a.a
-- id' = TmAbs "a" (Kind Star) $ TmAbs "x" (Var "a") (Var "x")
-- idt = Pi "a" (Kind Star) $ Pi "x" (Var "a") (Var "a")
-- foo = run stmt "let foo : Bool = True"
-- foo = run exprParser "foo (a : Bool) = @a"
foo = run exprParser "let id (a :: *) (x :: *) :: * = x in id;"
-- foo = run exprParser "let Bool :: *; Bool = forall (a :: *) . a -> a -> a; False :: Bool; in False"
-- foo = run exprParser "let x :: Int = g a ;y :: Int = f x in h x y;"
foo' = case foo of
    Right (expr) -> expr
    Left x       -> error "X"

foo'' = do
  t <- typeOf initialEnv foo'
  let (e, t') = t
  return t'

read' :: IO String
read' = putStr ">> " >> hFlush stdout >> getLine

eval' :: String -> String
eval' input = input

print' :: String -> IO ()
print' = putStrLn
