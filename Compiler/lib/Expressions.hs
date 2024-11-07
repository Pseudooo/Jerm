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

data Operator = OpAdd | OpSub | OpMul | OpDiv | OpEquals | OpAnd | OpOr | OpNot
    deriving (Show)

{-- 
    Expression parsing
--}
parseExpression :: Parser Expression
parseExpression = lexeme andExpression

andExpression :: Parser Expression
andExpression = chainl1 orExpression (BinaryExpression OpAnd <$ symbol "and")

orExpression :: Parser Expression
orExpression = chainl1 equalsExpression (BinaryExpression OpOr <$ symbol "or")

equalsExpression :: Parser Expression
equalsExpression = chainl1 notExpression (BinaryExpression OpEquals <$ symbol "==")

notExpression :: Parser Expression
notExpression = do
    negation <- optionMaybe $ symbol "not"
    innerExpression <- subExpression
    return $ case negation of
        Nothing -> innerExpression
        Just _ -> UnaryExpression OpNot innerExpression

subExpression :: Parser Expression
subExpression = chainl1 addExpression (BinaryExpression OpSub <$ symbol "-")

addExpression :: Parser Expression
addExpression = chainl1 mulExpression (BinaryExpression OpAdd <$ symbol "+")

mulExpression :: Parser Expression
mulExpression = chainl1 divExpression (BinaryExpression OpMul <$ symbol "*")

divExpression :: Parser Expression
divExpression = let base = (ConstantExpression <$> literal) <|> referenceExpression <|> parens parseExpression 
    in chainl1 base (BinaryExpression OpDiv <$ symbol "/")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

referenceExpression :: Parser Expression
referenceExpression = ReferenceExpression <$> (lexeme $ many1 letter)
