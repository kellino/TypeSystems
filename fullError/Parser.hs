module Parser (parseProgram) where

import Syntax

import Prelude hiding (error)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Unbound.Generics.LocallyNameless

import qualified Data.Text as T
import Control.Applicative (empty)
import Data.Scientific

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) [Term]
parseProgram = runParser prog

prog :: Parser [Term]
prog = between scn eof (sepEndBy expr scn)

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineCmnt empty

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "//"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Term
number = do
    n <- toRealFloat <$> lexeme L.number
    return $ TmNumber n

var :: Parser Term
var = do
    n <- some alphaNumChar <* sc
    return $ TmVar (string2Name n)

true :: Parser Term
true = rword "true" *> pure TmTrue

false :: Parser Term
false = rword "false" *> pure TmFalse

error :: Parser Term
error = rword "error" *> pure TmError

ifThen :: Parser Term
ifThen = do
    rword "if"
    b <- term
    rword "then"
    t1 <- term
    rword "else"
    t2 <- term
    return $ TmIf b t1 t2

lam :: Parser Term
lam = do
    void $ symbol "λ"
    name <- some alphaNumChar <* sc
    void $ symbol ":"
    tys <- typeDec `sepBy` symbol "→"
    void $ symbol "."
    body <- expr
    return $ TmAbs (bind (string2Name name, embed $ Annot $ Just (Type (wrap tys))) body)
    where wrap = foldl1 TyArr

typeDec :: Parser Ty
typeDec = do
    ty <- some alphaNumChar <* sc
    case ty of
         "Bool" -> return TyBool
         "Num" -> return TyNum
         "Var" -> return TyVar
         x -> fail $ "unrecognized primitive type " ++ show x

term :: Parser Term
term =  parens expr
    <|> true
    <|> false
    <|> ifThen
    <|> error
    <|> number
    <|> lam
    <|> var

expr :: Parser Term
expr = do
    es <- some term
    return $ foldl1 TmApp es
