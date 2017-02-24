{-# LANGUAGE TypeFamilies #-}

module Parser where

import Syntax

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T -- for testing in ghci, not needed for anything else


-----------------
-- Main Parser --
-----------------

type RawData t e = [Either (ParseError t e) Expr]

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) (RawData Char Dec)
parseProgram = runParser rawData

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
    where e = withRecovery recover (Right <$> expr)
          recover err = Left err <$ manyTill anyChar eol

-------------------------
-- General Combinators --
-------------------------

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineCmnt empty

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt empty

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "#"

lexeme :: Parser a -> Parser a 
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "true", "false"]

-- for this simple parser, let's only allow simple letter identifiers
legalChars :: String
legalChars = "abcdefghijklmnopqrstuvwxyz0123456789"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (oneOf legalChars)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- crude and ugly, but good enough for our needs here
sec :: Char -> Label
sec 'l' = Low
sec 'ₗ' = Low
sec 'm' = Medium
sec 'ₘ' = Medium
sec 'h' = High
sec 'ₕ' = High

------------------------
-- Expression Parsers --
------------------------

var :: Parser Expr
var = do
    v <- identifier
    labl <- (oneOf "ₕₗₘ" <|> braces (oneOf "hml")) <* sc
    return $ Var v (sec labl)

number :: Parser Expr
number = do
    n <- lexeme L.integer
    return $ Num n Low

doNothing :: Parser Expr
doNothing = do
    rword "skip"
    return $ Skip High 

assign :: Parser Expr
assign = do
    name <- var
    void $ symbol ":="
    body <- expr
    return $ Assign name body 

ifExpr :: Parser Expr
ifExpr = do
    rword "if"
    gd <- term
    rword "then"
    l <- term
    rword "else"
    r <- term
    return $ IfThenElse gd l r

while :: Parser Expr 
while = do
    rword "while"
    gd <- expr
    rword "do"
    body <- expr
    return $ While gd body

lattice :: Parser Expr
lattice = do
    rword "lattice"
    void $ symbol ":="
    str <- some (alphaNumChar <|> oneOf "{}, ") <* eol
    return $ Lattice str

-----------
-- Bools --
-----------

true :: Parser Expr
true = rword "true" *> pure (BoolExpr TmTrue Low)

false :: Parser Expr
false = rword "false" *> pure (BoolExpr TmFalse Low)

-------------------
-- Arith Parsers --
-------------------

arithExpr :: Parser Expr
arithExpr = makeExprParser term table <?> "expression"

table :: [[Operator Parser Expr]]
table = [[ infixOp ";" (Seq)
         , infixOp "+" (Op Add)
         , infixOp "-" (Op Sub)
         , infixOp "==" (Op Equal)
         , infixOp "≡" (Op Equal)
         , infixOp "<" (Op LessThan)
         , infixOp "<=" (Op LessThanEq)
         , infixOp "≤" (Op LessThanEq) ]]

infixOp x f = InfixL (symbol x >> return f)

------------------------
-- Expression parsers --
------------------------

term :: Parser Expr
term =  parens expr
    <|> lattice
    <|> doNothing
    <|> true
    <|> false
    <|> number
    <|> ifExpr
    <|> try assign
    <|> var
    <?> "term"

expr :: Parser Expr
expr = do
    es <- some (try arithExpr <|> term)
    return $ foldl1 App es
