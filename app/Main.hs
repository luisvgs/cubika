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
          deriving (Show)

data Stmt =
  Seq [Stmt]
  | Assign String Stmt
  | ExprStmt Expr deriving (Show)

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
integer = IntLit <$> lexeme L.decimal

boolean :: Parser Expr
boolean = do
  value <- choice [rword "True" *> pure True, rword "False" *> pure False]
  return (BoolLit value)

-- Parser for variable assignments
assignment :: Parser Stmt
assignment = do
  rword "let"
  name <- identifier
  space
  char '='
  space
  expr <- exprParser
  return (Assign name (ExprStmt expr))


stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt semicolon
  -- if there's only one stmt return it without using ‘Seq’
  where f l = if length l == 1 then head l else Seq l

stmt :: Parser Stmt
stmt = choice
  [ Seq <$> between (symbol "{") (symbol "}") (stmt `sepEndBy` semicolon)
  , assignment
  , ExprStmt <$> exprParser
  ]

-- stmt :: Parser Stmt
-- stmt = assignment <|> stmtSeq <|> ExprStmt <$> exprParser

exprParser :: Parser Expr
exprParser = integer <|> boolean

run :: Parser Stmt -> String -> Either (ParseErrorBundle String Void) Stmt
run parser = parse parser ""

-- main :: IO ()
-- main = getLine >>= print . run >> main
main = do
  input <- getLine
  case run stmt input of
    Left  err  -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right expr -> putStrLn $ "Parsed expression: " ++ show expr
