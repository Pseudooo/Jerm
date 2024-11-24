module Parsers.Utils(whitespace, lexeme, symbol) where

import Text.Parsec
import Text.Parsec.String

whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol s = lexeme (string s)
