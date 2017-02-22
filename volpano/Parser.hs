module Parser where

import Syntax

import Unbound.Generics.LocallyNameless (bind, string2Name, embed)
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T

--------------------
-- Program Parser -- 
--------------------

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) Phrase
parseProgram  = runParser phrase 

phrase :: Parser Phrase
phrase = do
    ex <- expression
    return $ Ex ex

-----------------
-- Conbinators -- 
-----------------

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

expression :: Parser Expr
expression = parens expression
          <|> number
          <|> add
          <|> equal
          <|> lessThan
          <|> var

number :: Parser Expr
number = do
    n <- lexeme L.integer
    return $ Num n

add :: Parser Expr
add = do
    n1 <- expression
    void (symbol "+")
    n2 <- expression
    return $ Add n1 n2

sub :: Parser Expr
sub = do
    n1 <- expression
    void (symbol "-")
    n2 <- expression
    return $ Sub n1 n2

equal :: Parser Expr
equal = do
    n1 <- expression
    void (symbol "=")
    n2 <- expression
    return $ Equal n1 n2

lessThan :: Parser Expr
lessThan = do
    n1 <- expression
    void (symbol "≤" <|> symbol "<=")
    n2 <- expression
    return $ Lt n1 n2

var :: Parser Expr
var = do
    v <- some alphaNumChar <* sc
    return $ Var (string2Name v)

---------------------
-- Command Parsers --
---------------------

command :: Parser Command
command = parens command
      <|> ifTerm
      <|> while
      <|> try letVar
      <|> assign
      <|> app
     
ifTerm :: Parser Command
ifTerm = do
    rword "if"
    ex <- expression
    rword "then"
    com1 <- command
    rword "else"
    com2 <- command
    return $ IfThenElse ex com1 com2
    
assign :: Parser Command
assign = do
    v <- var
    void (symbol ":=")
    e' <- expression
    return $ Assign v e'

while :: Parser Command
while = do
    rword "while"
    gd <- expression
    rword "do"
    body <- command
    return $ While gd body

letVar :: Parser Command
letVar = do
    rword "let"
    v <- some alphaNumChar <* sc
    void $ symbol ":="
    expr <- expression
    rword "in"
    body <- command
    return $ Let (bind (string2Name v, embed expr) body)

-- is this correct? 
app :: Parser Command
app = do
    cs <- some command
    return $ foldl1 App cs
