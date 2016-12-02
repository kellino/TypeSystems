{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}

module Parser where

import Syntax

import Prelude hiding (error)
import Unbound.Generics.LocallyNameless
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Data.Data
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T

-----------------------
-- CUSTOM ERROR TYPE --
-----------------------

type Parser = Parsec Cec T.Text

data Cec
  = CecFail String
  | CecIndentation Ordering Pos Pos
  | CecConversionError String
  deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent Cec where
  showErrorComponent (CecFail msg) = msg
  showErrorComponent (CecIndentation ord ref actual) =
    "incorrect indentation (got " ++ show (unPos actual) ++
    ", should be " ++ p ++ show (unPos ref) ++ ")"
    where p = case ord of
                LT -> "less than "
                EQ -> "equal to "
                GT -> "greater than "
  showErrorComponent (CecConversionError msg) =
    "conversion error: " ++ msg

instance ErrorComponent Cec where
  representFail        = CecFail
  representIndentation = CecIndentation

type RawData t e = [Either (ParseError t e) Term]

parseProgram :: String -> T.Text -> Either (ParseError Char Cec) [Either (ParseError Char Cec) Term]
parseProgram = runParser rawData

rawData :: Parser [Either (ParseError Char Cec) Term]
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

true :: Parser Term
true = reserved "true" *> pure TmTrue

false :: Parser Term
false = reserved "false" *> pure TmFalse

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

var :: Parser Term
var = do
    n <- some alphaNumChar <* sc
    return $ TmVar (string2Name n)

successor :: Parser Term
successor = do 
    reserved "succ" 
    num <- expr
    return $ TmSucc num

predecessor :: Parser Term
predecessor = do
    reserved "pred"
    num <- expr
    return $ TmPred num

error :: Parser Term
error = reserved "error" *> pure TmError

record :: Parser Term
record = do
    void $ symbol "{"
    fields <- recs `sepBy` symbol ","
    void $ symbol "}"
    return $ TmRecord fields
    where recs = do
              name <- some alphaNumChar <* sc
              void $ symbol "="
              val <- expr
              return (name, val)

zero :: Parser Term
zero = (reserved "0" <|> reserved "Z") *> pure TmZero

isZero :: Parser Term
isZero = do
    reserved "iszero" <|> reserved "0?"
    t <- expr
    return $ TmIsZero t

ifThenElse :: Parser Term
ifThenElse = do
    reserved "if"
    b <- term
    reserved "then"
    t1 <- term
    reserved "else"
    t2 <- term
    return $ TmIf b t1 t2

lam :: Parser Term
lam = do
    void $ symbol "λ" <|> symbol "\\"
    (TmVar n) <- var
    void $ symbol ":"
    ty <- some validChars `sepBy` (symbol "->" <|> symbol "→")
    void $ symbol "."
    body <- expr
    return $ TmAbs (bind (n, embed ty) body)
    where validChars = alphaNumChar <|> oneOf "{}:"

projection :: Parser Term 
projection = do
    rec <- record
    void $ symbol "."
    field <- some alphaNumChar <* sc
    return $ TmProj rec field

term :: Parser Term
term =  parens term
    <|> successor
    <|> predecessor
    <|> zero
    <|> isZero
    <|> error
    <|> lam
    <|> ifThenElse
    <|> true
    <|> false
    <|> try projection
    <|> record
    <|> var
    <?> "term"

expr :: Parser Term
expr = do
    es <- some term 
    return $ foldl1 TmApp es
