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
import Statements

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
    localsSize ss : maxStackSize ss : transpileStatements locals ss

transpileStatements :: [StackLocal] -> [Statement] -> [Int]
transpileStatements locals ss = foldr1 (++) (map (transpileStatement locals) ss)

transpileStatement :: [StackLocal] -> Statement -> [Int]
transpileStatement stackLocals (VariableInitialisation name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name]
transpileStatement stackLocals (VariableAssignment name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name]
transpileStatement stackLocals (IfStatement predicate body) = let transpiledBody = transpileStatements stackLocals body in
    transpileExpression stackLocals predicate ++ [11, length transpiledBody] 
transpileStatement stackLocals (IfElseStatement predicate ifBody elseBody) = transpiledPredicate ++ [11, length transpiledIfBody + 4] ++ transpiledIfBody ++ [10, length transpiledElseBody + 2] ++ transpiledElseBody
    where
        transpiledPredicate = transpileExpression stackLocals predicate
        transpiledIfBody = transpileStatements stackLocals ifBody
        transpiledElseBody = transpileStatements stackLocals elseBody

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

            IfStatement _ body -> let nestedAssignments = assignLocalOffsets' i body in
                nestedAssignments ++ assignLocalOffsets' (length nestedAssignments + i) xs

            IfElseStatement _ ifBody elseBody -> let ifBodyAssignments = assignLocalOffsets' i ifBody in
                let elseBodyAssignments = assignLocalOffsets' (length ifBodyAssignments + i) elseBody in
                    ifBodyAssignments ++ elseBodyAssignments ++ assignLocalOffsets' (length ifBodyAssignments + length elseBodyAssignments + i) xs

            _ -> assignLocalOffsets' i xs

transpileExpression :: [StackLocal] -> Expression -> [Int]
transpileExpression _ (ConstantExpression (IntegerLiteral x)) = 1 : x : []
transpileExpression _ (ConstantExpression (BooleanLiteral x)) = 1 : (case x == True of
    True -> 1
    False -> 0) : []
transpileExpression stackLocals (ReferenceExpression name) = 3 : getLocalOffset stackLocals name : []
transpileExpression stackLocals (BinaryExpression op left right) = transpileExpression stackLocals left ++ transpileExpression stackLocals right ++ [operatorOpcode op]
transpileExpression stackLocals (UnaryExpression op expr) = transpileExpression stackLocals expr ++ [operatorOpcode op]

operatorOpcode :: Operator -> Int
operatorOpcode Add = 4
operatorOpcode Sub = 5
operatorOpcode Equals = 6
operatorOpcode Not = 7
operatorOpcode And = 8
operatorOpcode Or = 9

localsSize :: [Statement] -> Int
localsSize (x:xs) = case x of
    (VariableInitialisation _ _) -> 1 + localsSize xs
    (IfStatement _ ifBody) -> localsSize ifBody + localsSize xs
    (IfElseStatement _ ifBody elseBody) -> localsSize ifBody + localsSize elseBody + localsSize xs
    _ -> localsSize xs
localsSize [] = 0

maxStackSize :: [Statement] -> Int
maxStackSize xs = foldr max 0 (map statementRequiredStackSize xs)

statementRequiredStackSize :: Statement -> Int
statementRequiredStackSize (VariableInitialisation _ expr) = expressionEvalStackSize expr
statementRequiredStackSize (VariableAssignment _ expr) = expressionEvalStackSize expr
statementRequiredStackSize (IfStatement predicate ifBody) = max (expressionEvalStackSize predicate) (maxStackSize ifBody)
statementRequiredStackSize (IfElseStatement predicate ifBody elseBody) = max (expressionEvalStackSize predicate) (max (maxStackSize ifBody) (maxStackSize elseBody))

expressionEvalStackSize :: Expression -> Int
expressionEvalStackSize (ReferenceExpression _) = 1
expressionEvalStackSize (ConstantExpression _) = 1
expressionEvalStackSize (BinaryExpression _ left right) = max (expressionEvalStackSize left) (expressionEvalStackSize right) + 1


