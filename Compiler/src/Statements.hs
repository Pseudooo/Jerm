module Statements where

import Text.Parsec
import Text.Parsec.String

import Expressions
import Utils

data Statement = VariableInitialisation String Expression
    | VariableAssignment String Expression
    deriving (Show)

statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement = initialisationStatement <|> assignmentStatement

initialisationStatement :: Parser Statement
initialisationStatement = lexeme $ do
    symbol "var"
    varName <- lexeme (many1 letter)
    symbol "="
    value <- parseExpression
    symbol ";"
    return $ VariableInitialisation varName value

assignmentStatement :: Parser Statement
assignmentStatement = lexeme $ do
    varName <- lexeme (many1 letter)
    symbol "="
    value <- parseExpression
    symbol ";"
    return $ VariableAssignment varName value
