module Main where
import           Cube

import           Control.Monad                  ( void )
import           Control.Monad.Combinators.Expr
import           Data.Text                      ( Text
                                                , singleton
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          | Assign String Expr
          deriving (Show)

-- Define the parser type
type Parser = Parsec Void String

spaces :: Parser ()
spaces = L.space (void spaceChar) lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: String -> Parser String
symbol = L.symbol spaces

semicolon :: Parser String
semicolon = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> spaces

rws :: [String] -- list of reserved words
rws = ["true", "false", "not", "and", "or", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

integer :: Parser Expr
integer = do
  value <- some digitChar
  return (IntLit (read value))

boolean :: Parser Expr
boolean = do
  value <- choice [string "True", string "False"]
  return (BoolLit (read value))

-- Parser for variable assignments
assignment :: Parser Expr
assignment = do
  rword "let"
  name <- identifier
  space
  char '='
  space
  expr <- exprParser
  return (Assign name expr)

-- Expression parser
exprParser :: Parser Expr
exprParser = integer <|> boolean <|> assignment

run :: Parser Expr -> String -> Either (ParseErrorBundle String Void) Expr
run parser = parse parser ""

-- main :: IO ()
-- main = getLine >>= print . run >> main
main = do
  input <- getLine
  case run exprParser input of
    Left  err  -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right expr -> putStrLn $ "Parsed expression: " ++ show expr
