module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Control.Monad (void)
import Data.Functor (($>))
import Ast

languageDef :: Token.LanguageDef ()
languageDef = emptyDef {
    Token.identStart = letter,
    Token.identLetter = letter,
    Token.reservedNames = ["let", "in", "True", "False", "if", "then", "else", "shift", "reset"],
    Token.caseSensitive = True
}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

sym :: String -> Parser ()
sym = void . Token.symbol lexer

res :: String -> Parser ()
res = void . Token.reserved lexer

true :: Parser Bool
true = res "True" $> True

false :: Parser Bool
false = res "False" $> False

name :: Parser Name
name = do
    s <- Token.identifier lexer
    return $ Name s 0

int :: Parser Int
int = do
    integer <- Token.natural lexer
    return $ fromInteger integer

parseVar :: Parser Expr
parseVar = Val . Var <$> name

parseAbs :: Parser Expr
parseAbs = Val <$> (Abs <$> (sym "\\" *> name <* sym ".") <*> parseExpr)

parseVInt :: Parser Expr
parseVInt = Val . VInt <$> int

parseVBool :: Parser Expr
parseVBool = Val . VBool <$> (true <|> false)

parseVLit :: Parser Expr
parseVLit = parseVInt <|> parseVBool 

parseBuiltinValue :: Parser Expr
parseBuiltinValue = sym "+" $> Val (Builtin $ BinOp Add)
                <|> sym "-" $> Val (Builtin $ BinOp Sub)
                <|> sym "*" $> Val (Builtin $ BinOp Mul)
                <|> sym "==" $> Val (Builtin $ BinOp Eq)
                <|> sym "&" $> Val (Builtin $ BinOp And)
                <|> sym "|" $> Val (Builtin $ BinOp Or)

parseApp :: Parser Expr
parseApp = chainl1 parseTerm (return App)

parseLet :: Parser Expr
parseLet = Let <$> (res "let" *> name) <*> (sym "=" *> parseExpr) <*> (res "in" *> parseExpr)

parseIfElse :: Parser Expr
parseIfElse = IfElse <$> (res "if" *> parseExpr) <*> (res "then" *> parseExpr) <*> (res "else" *> parseExpr)

parseShift :: Parser Expr
parseShift = Shift <$> (res "shift" *> name <* sym ".") <*> parseExpr

parseReset :: Parser Expr
parseReset = Reset <$> (res "reset" *> parseExpr)

parseParens :: Parser Expr
parseParens = sym "(" *> parseExpr <* sym ")"

parseTerm :: Parser Expr
parseTerm = parseParens <|> parseVar <|> parseAbs <|> parseVLit <|> parseBuiltinValue

parseExpr :: Parser Expr
parseExpr =  parseLet <|> parseIfElse <|> parseShift <|> parseReset <|> parseApp

parseLambda :: String -> Either ParseError Expr
parseLambda = parse (spaces *> parseExpr <* eof) "input-expr"
