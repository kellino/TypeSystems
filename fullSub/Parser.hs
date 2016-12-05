{-# LANGUAGE TypeFamilies #-}

module Parser where

import Syntax

import Prelude hiding (error)
import Unbound.Generics.LocallyNameless
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Scientific (toRealFloat)

import qualified Data.Text as T

type RawData t e = [Either (ParseError t e) Term]

parseProgram :: String -> T.Text -> Either (ParseError Char Dec) (RawData Char Dec)
parseProgram = runParser rawData

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
    where e = withRecovery recover (Right <$> expr)
          recover err = Left err <$ manyTill anyChar eol

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "true", "false"]

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

number :: Parser Term
number = do
    n <- toRealFloat <$> lexeme L.number
    return $ TmFloat n

floattimes :: Parser Term
floattimes = do
    rword "floattimes"
    n1 <- term
    n2 <- term
    return $ TmFloatTimes n1 n2

stringLit :: Parser Term
stringLit = do
    void $ symbol "\""
    str <- anyChar `manyTill` symbol "\""
    return $ TmString str

successor :: Parser Term
successor = do
    rword "succ"
    n <- expr
    return $ TmSucc n

predecessor :: Parser Term
predecessor = do
    rword "pred"
    n <- expr
    return $ TmPred n

zero :: Parser Term
zero = rword "0" *> pure TmZero

iszero :: Parser Term
iszero = do
    rword "iszero"
    t <- expr
    return $ TmIsZero t

true :: Parser Term
true = rword "true" *> pure TmTrue

false :: Parser Term
false = rword "false" *> pure TmFalse

var :: Parser Term
var = do
    n <- identifier
    return $ TmVar (string2Name n)

letRec :: Parser Term
letRec = do
    rword "rec"
    t1 <- expr
    return $ TmFix t1

lam :: Parser Term
lam = do
    void $ symbol "λ" <|> symbol "\\"
    (TmVar n) <- var
    void $ symbol ":"
    ty <- some validChars `sepBy` (symbol "->" <|> symbol "→")
    void $ symbol "."
    body <- expr
    return $ TmAbs (bind (n, embed ty) body)

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
    return $ TmIf b e1 e2

ascription :: Parser Term
ascription = do
    rword "alias"
    ty1 <- some validChars `sepBy` (symbol "->" <|> symbol "→") <* sc
    rword "as"
    ty2 <- some alphaNumChar <* sc
    return $ TmAscription ty1 ty2

error :: Parser Term
error = rword "error" *> pure TmError

term :: Parser Term
term =  parens expr
    <|> successor
    <|> predecessor
    <|> zero
    <|> iszero
    <|> ascription
    <|> lam
    <|> true
    <|> false
    <|> ifThenElse
    <|> error
    <|> stringLit
    <|> number
    <|> floattimes
    <|> letRec
    <|> var
    <?> "term"

expr :: Parser Term
expr = do
    tms <- some term 
    return $ foldl1 TmApp tms
