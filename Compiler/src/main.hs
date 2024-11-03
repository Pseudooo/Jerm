module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import System.Environment (getArgs)
import CommandArguments
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.Word

data Statement = VariableInitialisation String Expression
    | VariableAssignment String Expression
    deriving (Show)

data Expression = ConstantExpression ValueLiteral
    | BinaryExpression Operator Expression Expression
    | ReferenceExpression String
    deriving (Show)

data Operator = Add | Sub | Mul | Div
    deriving (Show)

data ValueLiteral = IntegerLiteral Integer
    deriving (Show)

type StackLocal = (String, Integer)

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

asWords :: [Integer] -> Put
asWords xs = mapM_ putWord32le (map fromIntegral xs)

{--
    Transpiler
--}
transpile :: [Statement] -> [Integer]
transpile ss = let locals = assignLocalOffsets ss in
    localsSize ss : maxStackSize ss : foldr1 (++) (map (transpileStatement locals) ss)

transpileStatement :: [StackLocal] -> Statement -> [Integer]
transpileStatement stackLocals (VariableInitialisation name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name]
transpileStatement stackLocals (VariableAssignment name expr) = transpileExpression stackLocals expr ++ [2] ++ [getLocalOffset stackLocals name] 

getLocalOffset :: [StackLocal] -> String -> Integer
getLocalOffset [] _ = -1
getLocalOffset ((name, offset):xs) requestedName
    | name == requestedName = offset
    | otherwise = getLocalOffset xs requestedName

assignLocalOffsets :: [Statement] -> [StackLocal]
assignLocalOffsets statements = assignLocalOffsets' 0 statements
    where
        assignLocalOffsets' :: Integer -> [Statement] -> [(String, Integer)]
        assignLocalOffsets' _ [] = []
        assignLocalOffsets' i (x:xs) = case x of
            VariableInitialisation name _ -> (name, i) : assignLocalOffsets' (i + 1) xs
            _ -> assignLocalOffsets' i xs

transpileExpression :: [StackLocal] -> Expression -> [Integer]
transpileExpression _ (ConstantExpression (IntegerLiteral x)) = 1 : x : []
transpileExpression stackLocals (ReferenceExpression name) = 3 : getLocalOffset stackLocals name : []
transpileExpression stackLocals (BinaryExpression _ left right) = transpileExpression stackLocals left ++ transpileExpression stackLocals right ++ [4]

localsSize :: [Statement] -> Integer
localsSize (x:xs) = case x of
    (VariableInitialisation _ _) -> 1 + localsSize xs
    _ -> localsSize xs
localsSize [] = 0

maxStackSize :: [Statement] -> Integer
maxStackSize xs = foldr max 0 (map statementRequiredStackSize xs)

statementRequiredStackSize :: Statement -> Integer
statementRequiredStackSize (VariableInitialisation _ expr) = expressionEvalStackSize expr
statementRequiredStackSize (VariableAssignment _ expr) = expressionEvalStackSize expr

expressionEvalStackSize :: Expression -> Integer
expressionEvalStackSize (ReferenceExpression _) = 1
expressionEvalStackSize (ConstantExpression _) = 1
expressionEvalStackSize (BinaryExpression _ left right) = max (expressionEvalStackSize left) (expressionEvalStackSize right) + 1

{--
    Utility Functions
--}
whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol s = lexeme (string s)

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

{-- 
    Expression parsing
--}
parseExpression :: Parser Expression
parseExpression = lexeme subExpression

subExpression :: Parser Expression
subExpression = chainl1 addExpression (BinaryExpression Sub <$ symbol "-")

addExpression :: Parser Expression
addExpression = chainl1 mulExpression (BinaryExpression Add <$ symbol "+")

mulExpression :: Parser Expression
mulExpression = chainl1 divExpression (BinaryExpression Mul <$ symbol "*")

divExpression :: Parser Expression
divExpression = chainl1 ((ConstantExpression <$> parseNumericLiteral) <|> (variableReference) <|> parens parseExpression) (BinaryExpression Div <$ symbol "/")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

variableReference :: Parser Expression
variableReference = ReferenceExpression <$> (lexeme $ many1 letter)

parseNumericLiteral :: Parser ValueLiteral
parseNumericLiteral = lexeme $ do
    sign <- optionMaybe (char '-')
    matchedInt <- many1 digit
    let parsedInt = read matchedInt
    return $ case sign of
        Nothing -> IntegerLiteral parsedInt
        Just _ -> IntegerLiteral $ negate parsedInt
