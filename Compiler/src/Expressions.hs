module Expressions where

import Text.Parsec
import Text.Parsec.String

import Literals
import Utils

data Expression = ConstantExpression ValueLiteral
    | BinaryExpression Operator Expression Expression
    | ReferenceExpression String
    deriving (Show)

data Operator = Add | Sub | Mul | Div
    deriving (Show)

{-- 
    Expression parsing
--}
parseExpression :: Parser Expression
parseExpression = lexeme subExpression

subExpression :: Parser Expression
subExpression = chainl1 addExpression (BinaryExpression Sub <$ symbol "-")

addExpression :: Parser Expression
addExpression = chainl1 mulExpression (BinaryExpression Add <$ symbol "+")

mulExpression :: Parser Expression
mulExpression = chainl1 divExpression (BinaryExpression Mul <$ symbol "*")

divExpression :: Parser Expression
divExpression = let base = (ConstantExpression <$> integerLiteral) <|> referenceExpression <|> parens parseExpression 
    in chainl1 base (BinaryExpression Div <$ symbol "/")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

referenceExpression :: Parser Expression
referenceExpression = ReferenceExpression <$> (lexeme $ many1 letter)
