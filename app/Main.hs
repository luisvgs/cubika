module Main where
import           Ast                            ( Stmt(..)
                                                , Term(..)
                                                , Type(..)
                                                )
import           Control.Monad                  ( unless )
import           Cube
import           Data.Either
import           Data.Void                      ( Void )
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
  case run stmt "let foo : Int = True" of
    Left  err  -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    -- Right expr -> print' $ show expr ++ "\n"
    Right expr -> print' $ show expr ++ "\n"
  -- repl

foo = run stmt "let foo : A = True"
foo' = case foo of
  Right (ExprStmt expr) -> expr
  Left  x               -> error "x"

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
