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
term =  unit
    <|> parens expr
    <|> ifTerm
    <|> boolTrue
    <|> boolFalse
    <|> abstraction
    <|> successor
    <|> predecessor
    <|> zero
    <|> iszero
    <|> var

expr :: Parser Term
expr = do
    es <- some term 
    return $ foldl1 App es

unit :: Parser Term
unit = rword "()" *> pure TmUnit

successor :: Parser Term
successor = do
    rword "succ" <|> rword "S"
    t1 <- expr
    return $ TmSucc t1

predecessor :: Parser Term
predecessor = do
    rword "pred" <|> rword "P"
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

types :: Parser String
types = symbol "Bool"
    <|> symbol "Nat"

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

abstraction :: Parser Term
abstraction = do
    void $ symbol "\\" 
    name <- some alphaNumChar
    void $ symbol ":"
    ty <- type'
    void $ symbol "."
    body <- expr
    return $ Abs (bind (string2Name name) body) ty

parseProgram :: String -> [String] -> Either (ParseError Char Dec) [Term]
parseProgram f = mapM $ runParser expr f
