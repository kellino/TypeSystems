{-# LANGUAGE TypeFamilies #-}

module Parser where

import Syntax

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
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

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineCmnt empty

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

successor :: Parser Term
successor = do 
    reserved "succ" 
    num <- expr
    return $ TmSucc num

zero :: Parser Term
zero = (reserved "0" <|> reserved "Z") *> pure TmZero

isZero :: Parser Term
isZero = do
    reserved "iszero" <|> reserved "0?"
    t <- expr
    return $ TmIsZero t

term :: Parser Term
term = parens expr
    <|> successor
    <|> zero
    <|> isZero

expr :: Parser Term
expr = do
    es <- some term <* sc
    return $ foldl1 TmApp es
