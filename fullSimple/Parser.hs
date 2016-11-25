module Parser where

import Syntax

import Data.Char (isUpper)
import Prelude hiding (succ, pred)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Unbound.Generics.LocallyNameless (string2Name, bind, embed)


parseProgram :: String -> [String] -> Either (ParseError Char Dec) [Term]
parseProgram f = mapM $ runParser expr f

-------------
-- PARSERS --
-------------

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt = L.skipLineComment "#"
          blockCmnt = L.skipBlockComment "(*" "*)"

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

float :: Parser Double
float = lexeme L.float 

double :: Parser Term
double = float >>= \f -> return $ TmFloat f

str :: Parser Term
str = do
    void $ symbol "\""
    s <- anyChar `manyTill` symbol "\""
    return $ TmString s

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

term :: Parser Term
term =  parens term
    <|> true
    <|> false
    <|> str
    <|> unit
    <|> succ
    <|> pred
    <|> ifThenElse
    <|> letIn
    <|> letRec
    <|> variant
    <|> zero
    <|> timesFloat
    <|> double
    <|> try projection
    <|> try record
    <|> tuple
    <|> lam
    <|> var

expr :: Parser Term
expr = do
    es <- some term 
    return $ foldl1 App es

typeDec :: Parser Type
typeDec = do
    ty <- constructor
    case ty of
         "Bool" -> return $ Type TyBool
         "Nat"  -> return $ Type TyNat
         "()" -> return $ Type TyUnit
         "String" -> return $ Type TyString
         "Float" -> return $ Type TyFloat
         _ -> fail "unknown type"

var :: Parser Term
var = do
    v <- some alphaNumChar <* spaceConsumer
    return $ Var (string2Name v)

lam :: Parser Term
lam = do
    void $ symbol "\\"
    name <- some alphaNumChar <* spaceConsumer
    void $ symbol ":"
    ty <- typeDec
    void $ symbol "."
    body <- expr
    return $ Lam (bind (string2Name name, embed $ Annot (Just ty)) body)

true :: Parser Term
true = rword "true" *> pure TmTrue

false :: Parser Term
false = rword "false" *> pure TmFalse

succ :: Parser Term
succ = do
    rword "succ"
    t1 <- expr
    return $ TmSucc t1

pred :: Parser Term
pred = do
    rword "pred"
    t1 <- expr
    return $ TmPred t1

zero :: Parser Term
zero = (rword "0" <|> rword "Z") *> pure TmZero

variant :: Parser Term
variant = do
    rword "type"
    name <- constructor
    void $ symbol ":="
    vars <- ntys `sepBy` symbol "|"
    return $ TmVariant name vars
    where ntys = do
              tag <- constructor
              ctype <- term
              return (tag, ctype)

constructor :: Parser String
constructor = do
    x@(n:_) <- some alphaNumChar <* spaceConsumer
    if isUpper n
       then return x
       else fail "a variant type must start with a capital letter"

letIn :: Parser Term
letIn = do
    rword "let"
    n <- some alphaNumChar <* spaceConsumer
    void $ symbol "="
    t <- term
    rword "in"
    body <- expr
    return $ Let (bind (string2Name n, embed t) body)

letRec :: Parser Term
letRec = do
    rword "rec"
    t1 <- expr
    return $ Fix t1

tuple :: Parser Term
tuple = do
    void $ symbol "{"
    rs <- term `sepBy` symbol ","
    void $ symbol "}"
    return $ TmRecord $ zip (map show ([1..]::[Int])) rs

record :: Parser Term
record = do
    void $ symbol "{"
    rs <- dec `sepBy` symbol ","
    void $ symbol "}"
    return $ TmRecord rs
    where dec = do
              v <- some alphaNumChar <* spaceConsumer
              void $ symbol "="
              val <- term
              return (v, val)

projection :: Parser Term
projection = do
    t1 <- try tuple <|> record 
    void $ symbol "."
    v <- some alphaNumChar <* spaceConsumer
    return $ TmProjection t1 v

timesFloat :: Parser Term
timesFloat = do
    rword "timesfloat"
    f1 <- double
    f2 <- double
    return $ TmTimesFloat f1 f2

ifThenElse :: Parser Term
ifThenElse = do
    rword "if"
    b <- term
    rword "then"
    t1 <- term
    rword "else"
    t2 <- term
    return $ If b t1 t2

unit :: Parser Term
unit = rword "()" *> pure TmUnit
