module Main where

import Test.HUnit
import Literals

tests :: [Test]
tests = TestList [
        TestLabel "IntegerListerlTest" testIntegerLiteral
    ]

testIntegerLiteral :: Test
testIntegerLiteral = TestCase $ assertEqual "Should parse Integer" (Right $ IntegerLiteral 1) (parse integerLiteral "" "1")

main :: IO ()
main = runTestTTAndExit tests