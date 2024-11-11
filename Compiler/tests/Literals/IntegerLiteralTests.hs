module Literals.IntegerLiteralTests where

import Test.HUnit
import Text.Parsec
import Data.Either
import Literals

integerLiteralTests :: Test
integerLiteralTests = TestList [
        parsePositiveIntegerLiteralTest,
        parseNegativeIntegerLiteralTest,
        failParseCharacterIntegerLiteralTest,
        failParseMixedCharacterIntegerLiteralTest
    ]

parsePositiveIntegerLiteralTest :: Test
parsePositiveIntegerLiteralTest = TestCase $ assertEqual "Should parse positive integer" 
    (Right . IntegerLiteral $ 12345) 
    (parse literal "" "12345")

parseNegativeIntegerLiteralTest :: Test
parseNegativeIntegerLiteralTest = TestCase $ assertEqual "Should parse positive integer" 
    (Right . IntegerLiteral $ -12345) 
    (parse literal "" "-12345")

failParseCharacterIntegerLiteralTest :: Test
failParseCharacterIntegerLiteralTest = TestCase $ assertEqual "Should fail parsing character string"
    True
    (isLeft $ parse literal "" "abc")

failParseMixedCharacterIntegerLiteralTest :: Test
failParseMixedCharacterIntegerLiteralTest = TestCase $ assertEqual "Should fail parsing a string with mix of digits & chars"
    True
    (isLeft $ parse literal "" "abc123")