module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.Word

import CommandArguments
import Utils
import Literals
import Expressions

data Statement = VariableInitialisation String Expression
    | VariableAssignment String Expression
    deriving (Show)

type StackLocal = (String, Int)

main :: IO ()
main = do
    options <- parseCommandArgs
    parseResult <- parseFromFile statements (source options)
    case parseResult of
        Left parseError -> (putStrLn . show $ parseError) >> return ()
        Right sourceTree -> do
            let byteCode = transpile sourceTree
            BS.writeFile (output options) (runPut $ asWords byteCode)
            putStrLn $ "Source code built to " ++ output options

asWords :: [Int] -> Put
asWords xs = mapM_ putWord32le (map fromIntegral xs)

{--
    Transpiler
--}
transpile :: [Statement] -> [Int]
transpile ss = let locals = assignLocalOffsets ss in
    localsSize ss : maxStackSize ss : foldr1 (++) (map (transpileStatement locals) ss)

transpileStatement :: [StackLocal] -> Statement -> [Int]
transpileStatement stackLocals (VariableInitialisation name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name]
transpileStatement stackLocals (VariableAssignment name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name] 

getLocalOffset :: [StackLocal] -> String -> Int
getLocalOffset [] _ = -1
getLocalOffset ((name, offset):xs) requestedName
    | name == requestedName = offset
    | otherwise = getLocalOffset xs requestedName

assignLocalOffsets :: [Statement] -> [StackLocal]
assignLocalOffsets statements = assignLocalOffsets' 0 statements
    where
        assignLocalOffsets' :: Int -> [Statement] -> [(String, Int)]
        assignLocalOffsets' _ [] = []
        assignLocalOffsets' i (x:xs) = case x of
            VariableInitialisation name _ -> (name, i) : assignLocalOffsets' (i + 1) xs
            _ -> assignLocalOffsets' i xs

transpileExpression :: [StackLocal] -> Expression -> [Int]
transpileExpression _ (ConstantExpression (IntegerLiteral x)) = 1 : x : []
transpileExpression stackLocals (ReferenceExpression name) = 3 : getLocalOffset stackLocals name : []
transpileExpression stackLocals (BinaryExpression _ left right) = transpileExpression stackLocals left ++ transpileExpression stackLocals right ++ [4]

localsSize :: [Statement] -> Int
localsSize (x:xs) = case x of
    (VariableInitialisation _ _) -> 1 + localsSize xs
    _ -> localsSize xs
localsSize [] = 0

maxStackSize :: [Statement] -> Int
maxStackSize xs = foldr max 0 (map statementRequiredStackSize xs)

statementRequiredStackSize :: Statement -> Int
statementRequiredStackSize (VariableInitialisation _ expr) = expressionEvalStackSize expr
statementRequiredStackSize (VariableAssignment _ expr) = expressionEvalStackSize expr

expressionEvalStackSize :: Expression -> Int
expressionEvalStackSize (ReferenceExpression _) = 1
expressionEvalStackSize (ConstantExpression _) = 1
expressionEvalStackSize (BinaryExpression _ left right) = max (expressionEvalStackSize left) (expressionEvalStackSize right) + 1

{-
    Statement parsing
-}
statements :: Parser [Statement]
statements = many statement

statement :: Parser Statement
statement = initialisationStatement <|> assignmentStatement

initialisationStatement :: Parser Statement
initialisationStatement = lexeme $ do
    symbol "var"
    varName <- lexeme (many1 letter)
    symbol "="
    value <- parseExpression
    symbol ";"
    return $ VariableInitialisation varName value

assignmentStatement :: Parser Statement
assignmentStatement = lexeme $ do
    varName <- lexeme (many1 letter)
    symbol "="
    value <- parseExpression
    symbol ";"
    return $ VariableAssignment varName value
