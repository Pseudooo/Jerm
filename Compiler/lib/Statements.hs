module Statements where

import Text.Parsec
import Text.Parsec.String

import Expressions
import Utils

data Statement = VariableInitialisation String Expression
    | VariableAssignment String Expression
    | IfStatement Expression [Statement]
    | IfElseStatement Expression [Statement] [Statement]
    deriving (Show, Eq)

statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement =  ifStatement <|> initialisationStatement <|> assignmentStatement

initialisationStatement :: Parser Statement
initialisationStatement = lexeme $ do
    varName <- symbol "var" >> lexeme (many1 letter)
    value <- symbol "=" >> parseExpression <* symbol ";"
    return $ VariableInitialisation varName value

assignmentStatement :: Parser Statement
assignmentStatement = lexeme $ do
    varName <- lexeme (many1 letter)
    value <- symbol "=" >> parseExpression <* symbol ";"
    return $ VariableAssignment varName value

ifStatement :: Parser Statement
ifStatement = lexeme $ do
    predicate <- (try $ symbol "if") >> lexeme parseExpression
    ifBody <- between (symbol "{") (symbol "}") statements
    elseBodyMaybe <- optionMaybe $ symbol "else" *> between (symbol "{") (symbol "}") statements
    return $ case elseBodyMaybe of 
        Nothing -> IfStatement predicate ifBody
        Just elseBody -> IfElseStatement predicate ifBody elseBody
