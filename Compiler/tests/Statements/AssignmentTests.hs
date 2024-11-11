module Statements.AssignmentTests where

import Test.HUnit
import Text.Parsec
import Data.Either
import Statements
import Expressions
import Literals

assignmentTests :: Test
assignmentTests = TestList [
        parseSimpleAssignmentTest,
        parseAssignmentNoTerminatorTest,
        parseAmbiguousAssignmentTest
    ]

parseSimpleAssignmentTest :: Test
parseSimpleAssignmentTest = TestCase $ assertEqual "Should parse simple assignment"
    (Right $ VariableAssignment "x" (ConstantExpression $ IntegerLiteral 1))
    (parse statement "" "x = 1;")

parseAssignmentNoTerminatorTest :: Test
parseAssignmentNoTerminatorTest = TestCase $ assertEqual "Should fail parsing with missing ;"
    True
    (isLeft $ parse statement "" "x = 1")

parseAmbiguousAssignmentTest :: Test
parseAmbiguousAssignmentTest = TestCase $ assertEqual "Should parse as assignment correctly and not fail as if"
    (Right $ VariableAssignment "i" (ConstantExpression $ IntegerLiteral 1))
    (parse statement "" "i = 1;")