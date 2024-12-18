module Parsers.Statements where

import Text.Parsec
import Text.Parsec.String

import Parsers.Expressions
import Parsers.Utils

data Statement = VariableInitialisation String Expression
    | VariableAssignment String Expression
    | IfStatement Expression [Statement]
    | IfElseStatement Expression [Statement] [Statement]
    | ForLoop Statement Expression Statement [Statement]
    deriving (Show, Eq)

statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement = ifStatement <|> forLoop <|> initialisationStatement <|> assignmentStatement

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

forLoop :: Parser Statement
forLoop = do
    _ <- try . symbol $ "for"
    initialisation <- symbol "(" >> statement
    condition <- parseExpression <* symbol ";"
    operation <- statement <* symbol ")"
    scope <- optionMaybe $ between (symbol "{") (symbol "}") statements
    case scope of
        Nothing -> do
            scopeSingle <- statement
            return $ ForLoop initialisation condition operation [scopeSingle]
        Just forBody -> return $ ForLoop initialisation condition operation forBody