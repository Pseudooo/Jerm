module Main where

import Text.Parsec.String
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put

import CommandArguments
import Parsers.Literals
import Parsers.Expressions
import Parsers.Statements
import Transpilers.ByteCodes
import Transpilers.LocalStack

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
    localsSize ss : maxStackSize ss : byteCodesAsInts (transpileStatements locals ss)

transpileStatements :: [StackLocal] -> [Statement] -> [ByteCode]
transpileStatements locals ss = foldr1 (++) (map (transpileStatement locals) ss)

transpileStatement :: [StackLocal] -> Statement -> [ByteCode]
transpileStatement stackLocals (VariableInitialisation name expr) = transpileExpression stackLocals expr ++ [STLOC $ getLocalOffset stackLocals name]
transpileStatement stackLocals (VariableAssignment name expr) = transpileExpression stackLocals expr ++ [STLOC $ getLocalOffset stackLocals name]
transpileStatement stackLocals (IfStatement predicate body) = let transpiledBody = transpileStatements stackLocals body in
    transpileExpression stackLocals predicate ++ [JMPNIF $ byteCodesSize transpiledBody]

transpileStatement stackLocals (IfElseStatement predicate ifBody elseBody) = transpiledPredicate ++ [JMPNIF $ byteCodesSize transpiledIfBody + 4] ++ transpiledIfBody ++ [JMP $ byteCodesSize transpiledElseBody + 2] ++ transpiledElseBody
    where
        transpiledPredicate = transpileExpression stackLocals predicate
        transpiledIfBody = transpileStatements stackLocals ifBody
        transpiledElseBody = transpileStatements stackLocals elseBody

transpileStatement stackLocals (ForLoop initStatement predicate operation body) = transpiledInitialisation 
        ++ transpiledPredicate 
        ++ [JMPNIF $ byteCodesSize transpiledBody + byteCodesSize transpiledOperation + 4] 
        ++ transpiledBody 
        ++ transpiledOperation 
        ++ [JMP . negate $ byteCodesSize transpiledPredicate + byteCodesSize transpiledBody + byteCodesSize transpiledOperation + 2]
    where
        transpiledInitialisation = transpileStatement stackLocals initStatement
        transpiledPredicate = transpileExpression stackLocals predicate
        transpiledOperation = transpileStatement stackLocals operation
        transpiledBody = transpileStatements stackLocals body

transpileExpression :: [StackLocal] -> Expression -> [ByteCode]
transpileExpression _ (ConstantExpression (IntegerLiteral x)) = LDCNST x : []
transpileExpression _ (ConstantExpression (BooleanLiteral x)) = LDCNST (case x of
    True -> 1
    False -> 0) : []
transpileExpression stackLocals (ReferenceExpression name) = LDLOC (getLocalOffset stackLocals name) : []
transpileExpression stackLocals (BinaryExpression op left right) = transpileExpression stackLocals left ++ transpileExpression stackLocals right ++ [operatorByteCode op]
transpileExpression stackLocals (UnaryExpression op expr) = transpileExpression stackLocals expr ++ [operatorByteCode op]




