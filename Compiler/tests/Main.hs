module Main where

import Test.HUnit
import Text.Parsec
import Literals.IntegerLiteralTests
import Statements.AssignmentTests
import Statements.IfStatementTests
import Transpilers.LocalStackOffsetTests

main :: IO ()
main = runTestTTAndExit compilerTests

compilerTests :: Test
compilerTests = TestList [
        integerLiteralTests,
        assignmentTests,
        ifStatementTests,
        localStackOffsetTests
    ]