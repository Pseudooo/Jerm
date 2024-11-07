module Literals.IntegerLiteralTests where

import Test.HUnit
import Text.Parsec
import Literals

integerLiteralTests :: Test
integerLiteralTests = TestList [
        TestLabel "Parse Positive Integer Literal" parsePositiveIntegerLiteralTest,
        TestLabel "Parse Negative Integer Literal" parseNegativeIntegerLiteralTest
    ]

parsePositiveIntegerLiteralTest :: Test
parsePositiveIntegerLiteralTest = TestCase $ assertEqual "Should parse positive integer" 
    (Right $ IntegerLiteral 12345) 
    (parse integerLiteral "" "12345")

parseNegativeIntegerLiteralTest :: Test
parseNegativeIntegerLiteralTest = TestCase $ assertEqual "Should parse positive integer" 
    (Right . IntegerLiteral $ -12345) 
    (parse integerLiteral "" "-12345")