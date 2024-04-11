module Parser where
import           Ast                            ( Kinds(..)
                                                , Stmt(..)
                                                , Term(..)
                                                , Type(..)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Combinators.Expr
import           Cube
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , singleton
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
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
rws = ["true", "false", "not", "and", "or", "let", "assume"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

integer :: Parser Term
integer = TmInt <$> lexeme L.decimal

boolean :: Parser Term
boolean = do
  value <- choice [rword "True" *> pure True, rword "False" *> pure False]
  return (BoolLit value)

function :: Parser Stmt
function = do
  name <- identifier
  spaces -- OJO, quiza sea space.
  char ':'
  space
  args <- identifier
  space
  char '='
  space
  exprs <- some exprParser
  return (Function name (Var args) (map ExprStmt exprs))

-- universe :: Parser Stmt
-- universe = do
--   rword "assume"
--   name <- identifier
--   space
--   char ':'
--   space
--   expr <- identifier
--   return (Universe name expr)

parseType :: Parser Type
parseType = choice [tyInt, tyBool, tyVar]

tyInt :: Parser Type
tyInt = string "Int" >> return TyInt

tyBool :: Parser Type
tyBool = string "Bool" >> return TyBool

tyKind :: Parser Type
tyKind = string "*" >> return (Kind Star)

tyVar :: Parser Type
tyVar = Var <$> identifier

variable :: Parser Term
variable = do
  char '@'
  s <- identifier
  return (Var s)


  -- let foo : Bool = True
assignment :: Parser Term
assignment = do
  rword "let"
  name <- identifier
  space
  char ':'
  space
  t <- parseType
  space
  char '='
  space
  expr <- exprParser
  return (Assign name t expr)

stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt semicolon
  where f l = if length l == 1 then head l else Seq l

stmt :: Parser Stmt
stmt = choice
  [ Seq <$> between (symbol "{") (symbol "}") (stmt `sepEndBy` semicolon)
  , ExprStmt <$> exprParser
  , function
  ]

exprParser :: Parser Term
exprParser = integer <|> boolean <|> variable <|> assignment

run :: Parser Stmt -> String -> Either (ParseErrorBundle String Void) Stmt
run parser = parse parser ""
