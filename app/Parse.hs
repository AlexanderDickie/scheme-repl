module Parse where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import LispVal 

symbol :: Parser Char
symbol = oneOf "!#$%&|*+/-:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space 

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseString :: Parser LispVal
parseString = do
    char '\"'
    x <- many (noneOf "\"")
    char '\"'
    return $ String x

parseList :: Parser LispVal
parseList = do
    char '('
    y <- liftM List $ sepBy parseExpr spaces
    -- liftM promotes function into a monad, so we may use in do block
    char ')'
    return y

parsePos :: Parser LispVal
parsePos = liftM (Number . read) $ many1 digit

parseNeg :: Parser LispVal
parseNeg = do
    char '-'
    pos <- (liftM read) $ many1 digit 
    return $ Number (-1 * pos) 

parseInteger :: Parser LispVal
parseInteger = parsePos <|> parseNeg

parseExpr :: Parser LispVal
parseExpr = try parseInteger <|>  parseAtom <|> parseString <|> parseList
-- -... may be an atom, so dont consume input on parseInteger (use try)

readExpr :: String -> Either ParseError LispVal
readExpr = parse parseExpr "lisp" 