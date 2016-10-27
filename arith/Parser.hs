module Parser where

import Text.Parsec
import Text.Parsec.String

import Control.Monad (void)
import Arith

boolTrue :: Parser Term
boolTrue = do
    void $ string "T" <* spaces
    return TmTrue

boolFalse :: Parser Term
boolFalse = do
    void $ string "F" <* spaces
    return TmFalse

ifThen :: Parser Term
ifThen = do
    void $ string "if" <* spaces
    b <- expr
    void $ spaces *> string "then" <* spaces
    t1 <- expr
    void $ spaces *> string "else" <* spaces
    t2 <- expr
    return $ TmIf b t1 t2

succ' :: Parser Term
succ' = do
    void $ string "S" <* spaces
    t1 <- expr
    return $ TmSucc t1

isZero :: Parser Term
isZero = do
    void $ string "Z?" <* spaces
    t1 <- expr
    return $ TmIsZero t1

zero :: Parser Term
zero = do
    void $ string "Z" <* spaces
    return TmZero

expr :: Parser Term
expr = try boolFalse
    <|> try boolTrue
    <|> ifThen
    <|> succ'
    <|> try isZero
    <|> zero

modl :: Parser [Term]
modl = many expr 

contents :: Parser a -> Parser a
contents p = do
  r <- p 
  eof
  return r

parseProgram :: SourceName -> String -> Either ParseError [Term]
parseProgram = parse (contents modl) 
