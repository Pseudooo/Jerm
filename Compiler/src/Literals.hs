module Literals where

import Text.Parsec
import Text.Parsec.String

import Utils

data ValueLiteral = IntegerLiteral Integer
    deriving (Show)

integerLiteral :: Parser ValueLiteral
integerLiteral = lexeme $ do
    sign <- optionMaybe (char '-')
    matchedInt <- many1 digit
    let parsedInt = read matchedInt
    return . IntegerLiteral $ case sign of
        Nothing -> parsedInt
        Just _ -> negate parsedInt