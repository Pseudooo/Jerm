module Main where

import Test.HUnit
import Text.Parsec
import Literals.IntegerLiteralTests

main :: IO ()
main = runTestTTAndExit integerLiteralTests