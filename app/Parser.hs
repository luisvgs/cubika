module Parser where

import           Ast                            (Kinds (..), Term (..),
                                                 Type (..))
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Cube
import           Data.Char                      (isAlpha, isAlphaNum)
import qualified Data.Map                       as Map
import           Data.Text                      (Text, singleton)
import           Data.Void                      (Void)
import           Debug.Trace                    (trace)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug

type Parser = Parsec Void String
type Sym = String

spaces :: Parser ()
spaces = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
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
rws = ["true", "false", "not", "and", "or", "let", "assume", "in", "forall"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

integer :: Parser Term
integer = TmInt <$> lexeme L.decimal

boolean :: Parser Term
boolean = do
    value <- choice [rword "True" *> pure True, rword "False" *> pure False]
    return (BoolLit value)

parseType :: Parser Term
parseType = exprParser

pLet :: Parser Term
pLet = do
    rword "let"
    stes <- sepBy pBind  (schar ';')
    optional (schar ';')
    rword "in"
    b <-  exprParser
    return $ eLets' stes b

pBind :: Parser (Sym, Type, Maybe Term) -- FIXME
pBind = try pBindH <|> pBindR


schar :: Char -> Parser ()
schar c = do
    spaces
    char c
    return ()

pBindH :: Parser (Sym, Type, Maybe Term)
pBindH = do
    sy <- dbg "pBindH" identifier
    lexeme (symbol "::")
    spaces
    ty <- parseType
    char ';'
    spaces
    sy' <- identifier
    as <- many identifier
    char '=' -- Esta cayendo mal
    b <- exprParser
    e <- matchH ty as b
    if sy /= sy' then
        error "oh no"
    else return (sy, ty, Just e)


pBindR :: Parser (Sym, Type, Maybe Term)
pBindR = do
    let addT (s, t) r = Pi s t r
        addE (s, t) e = TmAbs s t e
    sy <- dbg "pBindH" identifier
    args <- many pArg -- (a :: A) .. (b :: B)
    spaces
    symbol "::"
    rt <- parseType
    spaces
    (do
        char '='
        spaces
        be <- exprParser
        spaces
        return (sy, foldr addT rt args, Just $ foldr addE be args)
     ) <|>
       (return (sy, foldr addT rt args, Nothing))

matchH :: Term -> [Sym] -> Term -> Parser Term
matchH _ [] e = return e
matchH (Pi v t t') (a:as) e | v == a || v == "_" = do
    e' <- matchH t' as e
    return (TmAbs a t e')
matchH _ _ _ = error "ups"


eLet' :: (Sym, Type, Maybe Term) -> Term -> Term
eLet' (s, t, Nothing) b = TmAbs s t b
eLet' (s, t, Just e) b  = Let s t e b

eLets' :: [(Sym, Type, Maybe Term)] -> Term -> Term
eLets' stes b = foldr eLet' b stes

pArg :: Parser (Sym, Type) -- (a :: A) .. (b :: B)
pArg = pParen pVarType

pVarType :: Parser (Sym, Type) -- a :: A
pVarType = do
    s <- identifier
    symbol "::"
    spaces
    t <- parseType
    spaces
    return (s, t)

pParen :: Parser a -> Parser a
pParen = between (symbol "(") (symbol ")")

-- exprParser :: Parser Term
-- exprParser = pAExpr <|> integer <|> boolean <|> pPi <|> pLet

exprParser :: Parser Term
exprParser = try pLet <|> try pPi <|> try pApply <|> try pAtomExpr <|> integer <|> boolean

pPi :: Parser Term
pPi = pPiQuant <|> pPiArrow

pPiQuant :: Parser Term -- forall
pPiQuant = do
    rword "forall"
    sts <- some (pParen pVarType)
    lexeme (char '.')
    e <- parseType
    spaces
    return $ foldr (uncurry Pi) e sts

pPiArrow :: Parser Term
pPiArrow  = do
    ts <- some (do { e <- pPiArg; string "->"; return e })
    trace (show ts) (return ())
    rt <- pAExpr
    return $ foldr (\(s, t) r -> Pi s t r) rt ts
    where
        pPiArg = pPiNoDep <|> pArg
        pPiNoDep = do
            t <- pAExpr
            return ("_", t)

pAExpr :: Parser Term
pAExpr = pApply <|> pAtomExpr

pAtomExpr :: Parser Term
pAtomExpr = variable <|> pParen exprParser

variable :: Parser Term
variable = do
    s <- identifier
    return (Var s)

pApply :: Parser Term
pApply = do
    f <- pAtomExpr
    as <- some pAtomExpr
    return $ foldl TmApp f as

parseTop :: Parser Term
parseTop = do
    e <- exprParser
    optional spaces
    return e

-- Define the parser type
run :: Parser Term -> String -> Either (ParseErrorBundle String Void) Term
run parser = parse parser ""
