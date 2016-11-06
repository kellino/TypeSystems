module Parser (runTerm) where

import Syntax
import Eval

import Control.Arrow ((+++))
import Unbound.Generics.LocallyNameless (bind, string2Name)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellDef)

lexer :: Tok.TokenParser () 
lexer      = Tok.makeTokenParser haskellDef

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens     = Tok.parens lexer

ident :: Parser String
ident      = Tok.identifier lexer

var :: Parser Term
var = do
    x <- ident
    return $ Var (string2Name x)

lambda :: Parser Term
lambda = do
    reservedOp "\\"
    x <- ident
    reservedOp "."
    t <- parseTerm
    return $ Lam (bind (string2Name x) t)

parseTerm :: Parser Term
parseTerm = expr `chainl1` pure App

expr :: Parser Term
expr = 
        parens parseTerm
    <|> var
    <|> lambda

runTerm :: String -> Either ParseError Term
runTerm = (id +++ eval) . parse parseTerm "<from file>"
