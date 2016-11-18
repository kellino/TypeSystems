module Parser (parseProgram) where

import Syntax

import Unbound.Generics.LocallyNameless (bind, string2Name)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "(*" "*)"

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

term :: Parser Term
term =  parens expr
    <|> ifTerm
    <|> boolTrue
    <|> boolFalse
    <|> var
    <|> abstraction

expr :: Parser Term
expr = do
    es <- some term 
    return $ foldl1 App es

boolTrue :: Parser Term
boolTrue = do
    rword "true" 
    return TmTrue

boolFalse :: Parser Term
boolFalse = do
    rword "false"
    return TmFalse

ifTerm :: Parser Term
ifTerm = do
    rword "if"
    cond <- term
    rword "then"
    e1 <- term
    rword "else"
    e2 <- term
    return $ If cond e1 e2

var :: Parser Term
var = do
    v <- some alphaNumChar <* sc
    return $ Var (string2Name v)

typeArrow :: Parser Ty
typeArrow = do
    arrs <- varType `sepBy` symbol "->"
    return $ foldl1 TyArr arrs

varType :: Parser Ty
varType = do
    rword "Bool" 
    return TyBool

abstraction :: Parser Term
abstraction = do
    void $ symbol "\\" 
    name <- some alphaNumChar
    void $ symbol ":"
    ty <- try typeArrow <|> varType 
    void $ symbol "."
    body <- expr
    return $ Abs (bind (string2Name name) body)

parseProgram :: String -> [String] -> Either (ParseError Char Dec) [Term]
parseProgram f = mapM $ runParser expr f
