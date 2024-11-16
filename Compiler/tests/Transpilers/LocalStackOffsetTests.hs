module Transpilers.LocalStackOffsetTests where

import Test.HUnit
import Parsers.Statements
import Parsers.Expressions
import Parsers.Literals
import Transpilers.LocalStack

localStackOffsetTests :: Test
localStackOffsetTests = TestList [
        forLoopInitialisationLocalsSize,
        forLoopLocalsSizeTest
    ]

forLoopInitialisationLocalsSize :: Test
forLoopInitialisationLocalsSize = TestCase $ assertEqual "Should identify initialisation step of for-loop in computed locals size"
    1
    (localsSize $ ForLoop 
        (VariableInitialisation "i" (ConstantExpression $ IntegerLiteral 0)) 
        (ConstantExpression $ BooleanLiteral True)
        (VariableAssignment "i" (ConstantExpression $ IntegerLiteral 0))
        [
            VariableAssignment "x" (ConstantExpression $ IntegerLiteral 0)
        ] : [])

forLoopLocalsSizeTest :: Test
forLoopLocalsSizeTest = TestCase $ assertEqual "Should identify locals size with variable defined within for-loop's body"
    2
    (localsSize $ ForLoop 
        (VariableInitialisation "i" (ConstantExpression $ IntegerLiteral 0)) 
        (ConstantExpression $ BooleanLiteral True)
        (VariableAssignment "i" (ConstantExpression $ IntegerLiteral 0))
        [
            VariableInitialisation "x" (ConstantExpression $ IntegerLiteral 0)
        ] : [])
    