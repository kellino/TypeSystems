module Parser where

import Syntax

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T -- for testing in ghci, not needed for anything else

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

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

------------------------
-- Expression Parsers --
------------------------

var :: Parser Expr
var = do
    v <- some alphaNumChar <* sc
    return $ Var v

number :: Parser Expr
number = do
    n <- lexeme L.integer
    return $ Num n

doNothing :: Parser Expr
doNothing = rword "skip" *> return Skip

assign :: Parser Expr
assign = do
    name <- var
    void $ symbol ":="
    body <- expr
    return $ Assign name body

ifExpr :: Parser Expr
ifExpr = do
    rword "if"
    gd <- expr
    rword "then"
    l <- expr
    rword "else"
    r <- expr
    return $ IfThenElse gd l r

while :: Parser Expr
while = do
    rword "while"
    gd <- expr
    rword "do"
    body <- expr
    return $ While gd body

-----------
-- Bools --
-----------

true :: Parser Expr
true = rword "true" *> pure (BoolExpr TmTrue)

false :: Parser Expr
false = rword "false" *> pure (BoolExpr TmFalse)

-------------------
-- Arith Parsers --
-------------------

arithExpr :: Parser Expr
arithExpr = makeExprParser term table <?> "expression"

table :: [[Operator Parser Expr]]
table = [ [ InfixL (Add <$ symbol "+")  
         ,  InfixL (Sub <$ symbol "-") ] ]

-----------------
-- Main parsers --
------------------

-- main expression parser
term :: Parser Expr
term =  parens expr
    <|> true
    <|> false
    <|> number
    <|> ifExpr
    -- <|> arithExpr
    <|> var
    <?> "term"

expr :: Parser Expr
expr = do
    es <- some term
    return $ foldl1 App es
