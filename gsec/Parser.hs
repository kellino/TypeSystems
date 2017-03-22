{-# LANGUAGE TypeFamilies #-}

module Parser where

import Syntax
import Lattice

import Unbound.Generics.LocallyNameless
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import qualified Data.Text as T

type RawData t e = [Either (ParseError t e) Term]

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) [Either (ParseError Char Dec) Binding]
parseProgram = runParser rawData

-- rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
    where e = withRecovery recover (Right <$> decl)
          recover err = Left err <$ manyTill anyChar eol

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "true", "false", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineCmnt empty

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt empty

lexeme :: Parser a -> Parser a 
lexeme = L.lexeme sc
 
symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "#"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

labels :: Parser String
labels = string "L" 
     <|> string "MA" 
     <|> string "MB" 
     <|> string "H" 
     <|> string "Any"

match :: String -> GLabel
match "L" = L
match "MA" = MA
match "MB" = MB
match "H" = H
match "Any" = Any

true :: Parser Term
true = do
    rword "true"
    lbl <- optional $ braces labels
    case lbl of
         Nothing -> return $ Val TmTrue Any
         Just l -> return $ Val TmTrue (match l)

false :: Parser Term
false = do
    rword "false"
    lbl <- optional $ braces labels
    case lbl of
         Nothing -> return $ Val TmFalse Any
         Just l -> return $ Val TmFalse (match l)

var :: Parser Term
var = do
    n <- identifier
    return $ Var (string2Name n)

variable :: Parser TName
variable = do
    i <- identifier
    return $ string2Name i

ifThenElse :: Parser Term
ifThenElse = do
    rword "if"
    b <- expr
    rword "then"
    e1 <- expr 
    rword "else"
    e2 <- expr 
    return $ IfThenElse b e1 e2

-----------------------
-- Boolean Operators --
-----------------------

boolExpr :: Parser Term
boolExpr = makeExprParser term table <?> "expression"

table :: [[Operator Parser Term]]
table = [[ infixOp "->" Implies
         , infixOp "/\\" And 
         , infixOp "\\/" Or ]]

infixOp x f = InfixL (symbol x >> return (Op f))

term :: Parser Term
term =  parens expr
    <|> true
    <|> false
    <|> ifThenElse
    <|> var
    <?> "term"

expr :: Parser Term
expr = do
    tms <- some (try boolExpr <|> term)
    return $ foldl1 App tms

-------------------------
-- Declaration Parsers --
-------------------------

type Binding = (String, Term)

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

letDecl :: Parser Binding
letDecl = do
    rword "let"
    name <- identifier
    lbl <- optional $ braces labels
    binds <- many variable
    void $ symbol "="
    body <- expr
    return (name, foldr lambda body binds)
    where lambda x m = Lam (bind (x, embed $ Annot $ Just Any) m)

decl :: Parser Binding
decl = letDecl <|> val <?> "a declaration"
