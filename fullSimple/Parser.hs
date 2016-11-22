module Parser (parseProgram) where

import Syntax

import Unbound.Generics.LocallyNameless (bind, string2Name)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-------------------
-- Main function --
-------------------

parseProgram :: String -> [String] -> Either (ParseError Char Dec) [Term]
parseProgram f = mapM $ runParser expr f

-------------
-- PARSERS --
------------

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt = L.skipLineComment "#"
          blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

term :: Parser Term
term =  unit
    <|> parens expr
    <|> try recordLookup
    <|> record
    <|> str
    <|> ifTerm
    <|> boolTrue
    <|> boolFalse
    <|> abstraction
    <|> successor
    <|> predecessor
    <|> zero
    <|> iszero
    <|> double
    <|> timesFloat
    <|> letTerm
    <|> var

expr :: Parser Term
expr = do
    es <- some term 
    return $ foldl1 App es

float :: Parser Double
float = lexeme L.float

double :: Parser Term
double = float >>= \f -> return $ TmFloat f

timesFloat :: Parser Term
timesFloat = do
    void $ symbol "timesfloat"
    f1 <- term
    f2 <- term
    return $ TmTimesFloat f1 f2

record :: Parser Term
record = do
    void $ symbol "{"
    contents <- dec `sepBy` symbol ","
    void $ symbol "}"
    return $ TmRecord contents ""

recordLookup :: Parser Term
recordLookup = do
    void $ symbol "{"
    contents <- dec `sepBy` symbol ","
    void $ symbol "}"
    void $ symbol "."
    field <- some alphaNumChar <* sc
    return $ TmRecord contents field

dec :: Parser (String, Term)
dec = do
    name <- some alphaNumChar <* sc
    void $ symbol ":="
    body <- expr
    return (name, body)

str :: Parser Term
str = do
    void $ symbol "\""
    st <- anyChar `someTill` symbol "\""
    return $ TmString st

unit :: Parser Term
unit = rword "()" *> pure TmUnit

letTerm :: Parser Term
letTerm = do
    void $ rword "let"
    name <- some alphaNumChar <* sc
    void $ symbol ":="
    sub <- term
    void $ rword "in"
    body <- expr
    return $ Let name sub body

successor :: Parser Term
successor = do
    rword "succ" 
    t1 <- expr
    return $ TmSucc t1

predecessor :: Parser Term
predecessor = do
    rword "pred" 
    t1 <- expr
    return $ TmPred t1

zero :: Parser Term
zero = do
    rword "0" <|> rword "Z"
    return TmZero

iszero :: Parser Term
iszero = do
    rword "0?" <|> rword "Z?"
    t1 <- expr
    return $ TmIsZero t1

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

abstraction :: Parser Term
abstraction = do
    void $ symbol "\\" 
    name <- some alphaNumChar
    void $ symbol ":"
    ty <- type'
    void $ symbol "."
    body <- expr
    return $ Abs (bind (string2Name name) body) ty

-----------
-- TYPES --
-----------

types :: Parser String
types = symbol "Bool"
    <|> symbol "Nat"
    <|> symbol "Unit"

type' :: Parser Ty
type' = do
    tys <- types `sepBy` symbol "->"
    let tys' = map setType tys
    return $ foldl1 TyArr tys'

setType :: String -> Ty
setType x = 
    case x of
         "Bool" -> TyBool
         "Nat"  -> TyNat
         "Unit" -> TyUnit
         err      -> error $ "unknown type: " ++ show err

