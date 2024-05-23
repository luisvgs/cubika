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
spaces = L.space space1 lineCmnt blockCmnt
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
rws = ["true", "false", "not", "and", "or", "let", "assume", "in", "forall", "_", "Type"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

integer :: Parser Term
integer = TmInt <$> lexeme L.decimal

boolean :: Parser Term
boolean = do
    value <- choice [rword "true" *> pure True, rword "false" *> pure False]
    return (Kind Star)

parseType :: Parser Term
parseType = try pPi <|> pAExpr

pLet :: Parser Term
pLet = do
    rword "let"
    stes <- sepBy pBind semicolon
    optional semicolon
    optional (rword "in")
    b <- exprParser
    return $ eLets' stes b

pBind :: Parser (Sym, Term, Maybe Term)
pBind = try pBindH <|> try pBindR

pBindH :: Parser (Sym, Term, Maybe Term)
pBindH = do
    sy <- identifier
    lexeme (symbol ":")
    ty <- parseType
    semicolon
    sy' <- identifier
    as <- many identifier
    char '='
    b <- exprParser
    e <- matchH ty as b
    if sy /= sy' then
        fail "Identifiers do not match"
    else return (sy, ty, Just e)

pBindR :: Parser (Sym, Term, Maybe Term)
pBindR = do
    let addT (s, t) r = Pi s t r
        addE (s, t) e = TmAbs s t e
    sy <- identifier
    args <- many pArg
    symbol ":"
    rt <- parseType
    trace (show "bindR") (return ())
    trace (show "parsed type: " ++ show rt) (return ())
    (do
        char '='
        spaces
        trace (show "ehm? ") (return ())
        be <- exprParser
        spaces
        return (sy, foldr addT rt args, Just $ foldr addE be args)
     ) <|>
        return  (sy, foldr addT rt args, Nothing)

matchH :: Term -> [Sym] -> Term -> Parser Term
matchH _ [] e = return e
matchH (Pi v t t') (a:as) e | v == a || v == "_" = do
    e' <- matchH t' as e
    return (TmAbs a t e')
matchH _ _ _ = fail "Mismatch in matchH"

eLet' :: (Sym, Term, Maybe Term) -> Term -> Term
eLet' (s, t, Nothing) b = TmAbs s t b
eLet' (s, t, Just e) b  = Let s t e b

eLets' :: [(Sym, Term, Maybe Term)] -> Term -> Term
eLets' stes b = foldr eLet' b stes

pArg :: Parser (Sym, Term)
pArg = pParen pVarType

pVarType :: Parser (Sym, Term)
pVarType = do
    s <- identifier
    symbol ":"
    t <- parseType
    return (s, t)

pParen :: Parser a -> Parser a
pParen = between (symbol "(") (symbol ")")

exprParser :: Parser Term
exprParser = try pLet <|> try pPi <|> try pLam <|> try pApply <|> try pAtomExpr

pPi :: Parser Term
pPi = try pPiQuant <|> pPiArrow

pPiQuant :: Parser Term
pPiQuant = do
    rword "forall"
    sts <- some (pParen pVarType)
    lexeme (char '.')
    e <- parseType
    spaces
    return $ foldr (uncurry Pi) e sts

pPiArrow :: Parser Term
pPiArrow = do
    ts <- some (try pPiArg <|> pParenArg)
    symbol "->"
    rt <- parseType
    spaces
    return $ foldr (\(s, t) r -> Pi s t r) rt ts
  where
    pPiArg = pArg
    pParenArg = do
        t <- pAExpr
        return ("_", t)

pLam :: Parser Term
pLam = do
    char '\\'
    sts <- fmap (:[]) pVarType <|> many (pParen pVarType)
    string "->"
    e <- exprParser
    return $ foldr (uncurry TmAbs) e sts

pAExpr :: Parser Term
pAExpr = try pApply <|> pAtomExpr

pAtomExpr :: Parser Term
pAtomExpr = variable <|> parseKind <|> pParen exprParser <|> integer <|> boolean

parseKind :: Parser Term
parseKind = do
    (do rword "Type"; return $ Kind Star) <|> (do string "[]"; return $ Kind Box)

variable :: Parser Term
variable = Var <$> identifier

pApply :: Parser Term
pApply = do
    f <- pAtomExpr
    as <- some pAtomExpr
    return $ foldl TmApp f as

run :: Parser Term -> String -> Either (ParseErrorBundle String Void) Term
run parser = parse parser ""
