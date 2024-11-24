module Statements.IfStatementTests where

import Test.HUnit
import Text.Parsec
import Data.Either
import Parsers.Statements
import Parsers.Expressions
import Parsers.Literals

ifStatementTests :: Test
ifStatementTests = TestList [
        parseIfStatement,
        parseIfElseStatement
    ]

parseIfStatement :: Test
parseIfStatement = TestCase $ assertEqual "Should parse if statement"
    (Right $ IfStatement (ConstantExpression $ BooleanLiteral True) [VariableAssignment "x" (ConstantExpression $ IntegerLiteral 1)])
    (parse statement "" "if True { x = 1; }")

parseIfElseStatement :: Test
parseIfElseStatement = TestCase $ assertEqual "Should parse if else statement"
    (Right $ IfElseStatement (ConstantExpression $ BooleanLiteral True) 
        [VariableAssignment "x" (ConstantExpression $ IntegerLiteral 1)] 
        [VariableAssignment "y" (ConstantExpression $ IntegerLiteral 2)])
    (parse statement "" "if True { x = 1; } else { y = 2; }")