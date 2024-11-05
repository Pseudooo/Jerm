module Expressions where

import Text.Parsec
import Text.Parsec.String

import Literals
import Utils

data Expression = ConstantExpression ValueLiteral
    | BinaryExpression Operator Expression Expression
    | UnaryExpression Operator Expression
    | ReferenceExpression String
    deriving (Show)

data Operator = Add | Sub | Mul | Div | Equals | And | Or | Not
    deriving (Show)

{-- 
    Expression parsing
--}
parseExpression :: Parser Expression
parseExpression = lexeme andExpression

andExpression :: Parser Expression
andExpression = chainl1 orExpression (BinaryExpression And <$ symbol "and")

orExpression :: Parser Expression
orExpression = chainl1 equalsExpression (BinaryExpression Or <$ symbol "or")

equalsExpression :: Parser Expression
equalsExpression = chainl1 notExpression (BinaryExpression Equals <$ symbol "==")

notExpression :: Parser Expression
notExpression = do
    negation <- optionMaybe $ symbol "not"
    innerExpression <- subExpression
    return $ case negation of
        Nothing -> innerExpression
        Just _ -> UnaryExpression Not innerExpression

subExpression :: Parser Expression
subExpression = chainl1 addExpression (BinaryExpression Sub <$ symbol "-")

addExpression :: Parser Expression
addExpression = chainl1 mulExpression (BinaryExpression Add <$ symbol "+")

mulExpression :: Parser Expression
mulExpression = chainl1 divExpression (BinaryExpression Mul <$ symbol "*")

divExpression :: Parser Expression
divExpression = let base = (ConstantExpression <$> literal) <|> referenceExpression <|> parens parseExpression 
    in chainl1 base (BinaryExpression Div <$ symbol "/")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

referenceExpression :: Parser Expression
referenceExpression = ReferenceExpression <$> (lexeme $ many1 letter)
