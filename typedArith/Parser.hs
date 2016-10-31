module Parser (parseProgram) where 

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

-- import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart = "(*"
    , Tok.commentEnd = "*)"
    , Tok.commentLine = "--"
    , Tok.nestedComments = True
    , Tok.identStart = letter
    , Tok.identLetter = alphaNum <|> oneOf "_'"
    , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames = []
    , Tok.reservedOpNames = []
    , Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

ifthen :: Parser Term
ifthen = do
    reserved "if"
    cond <- expr
    reserved "then"
    t1 <- expr
    reserved "else"
    t2 <- expr
    return $ TmIf cond t1 t2

successor :: Parser Term
successor = do
    reserved "succ" <|> reserved "S"
    t1 <- expr
    return $ TmSucc t1

predecessor :: Parser Term
predecessor = do
    reserved "pred" <|> reserved "P"
    t1 <- expr
    return $ TmPred t1

zero :: Parser Term
zero = do
    reserved "0" <|> reserved "Z"
    return TmZero

iszero :: Parser Term
iszero = do
    reserved "0?" <|> reserved "Z?"
    t1 <- expr
    return $ TmIsZero t1

true, false :: Parser Term
true = reserved "true" >> return TmTrue
false = reserved "false" >> return TmFalse

expr :: Parser Term
expr =  true
    <|> false
    <|> try iszero
    <|> zero
    <|> ifthen
    <|> successor
    <|> predecessor
    <|> parens expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof 
    return r

toplevel :: Parser [Term]
toplevel = many expr

parseProgram :: SourceName -> String -> Either ParseError [Term]
parseProgram = parse (contents toplevel) 
