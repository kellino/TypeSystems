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

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) (RawData Char Dec)
parseProgram = runParser rawData

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
    where e = withRecovery recover (Right <$> expr)
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

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "#"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

true :: Parser Term
true = rword "true" *> pure (Val TmTrue L)

false :: Parser Term
false = rword "false" *> pure (Val TmFalse L)

var :: Parser Term
var = do
    n <- identifier
    return $ Var (string2Name n)

lam :: Parser Term
lam = do
    rword "let"
    (Var n) <- var
    void $ symbol "="
    body <- expr
    return $ Lam (bind (n, embed $ Annot Nothing) body)
    {-void $ symbol "λ" <|> symbol "\\"-}
    {-(Var n) <- var-}
    {-void $ symbol ":"-}
    {-ty <- some validChars `sepBy` (symbol "->" <|> symbol "→")-}
    {-void $ symbol "."-}
    {-body <- expr-}
    {-return $ Abs (bind (n, embed ty) body)-}

validChars :: Parser Char
validChars = alphaNumChar <|> oneOf "{:}"

ifThenElse :: Parser Term
ifThenElse = do
    rword "if"
    b <- expr
    rword "then"
    e1 <- expr 
    rword "else"
    e2 <- expr 
    return $ IfThenElse b e1 e2

boolExpr :: Parser Term
boolExpr = makeExprParser term table <?> "expression"

table :: [[Operator Parser Term]]
table = [[ infixOp "->" Implies
         , infixOp "/\\" And 
         , infixOp "\\/" Or ]]

infixOp x f = InfixL (symbol x >> return (Op f))

term :: Parser Term
term =  parens expr
    <|> lam
    <|> true
    <|> false
    <|> ifThenElse
    <|> var
    <?> "term"

expr :: Parser Term
expr = do
    tms <- some (try boolExpr <|> term)
    return $ foldl1 App tms
