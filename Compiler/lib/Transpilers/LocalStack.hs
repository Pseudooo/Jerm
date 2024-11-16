module Transpilers.LocalStack where

import Parsers.Statements
import Parsers.Expressions

type StackLocal = (String, Int)

getLocalOffset :: [StackLocal] -> String -> Int
getLocalOffset [] _ = -1
getLocalOffset ((name, offset):xs) requestedName
    | name == requestedName = offset
    | otherwise = getLocalOffset xs requestedName

assignLocalOffsets :: [Statement] -> [StackLocal]
assignLocalOffsets stmts = assignLocalOffsets' 0 stmts
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

            ForLoop initialisation _ _ forBody -> let initialisationLocalOffset = assignLocalOffsets' i [initialisation] in
                let bodyLocalOffsets = assignLocalOffsets' (length bodyLocalOffsets + i) forBody in
                    initialisationLocalOffset ++ bodyLocalOffsets ++ assignLocalOffsets' (length initialisationLocalOffset + length bodyLocalOffsets + i) xs

            _ -> assignLocalOffsets' i xs

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
statementRequiredStackSize (ForLoop initialisation predicate operation body) = max (statementRequiredStackSize initialisation) (max (expressionEvalStackSize predicate) (max (statementRequiredStackSize operation) (maxStackSize body)))    

expressionEvalStackSize :: Expression -> Int
expressionEvalStackSize (ReferenceExpression _) = 1
expressionEvalStackSize (ConstantExpression _) = 1
expressionEvalStackSize (BinaryExpression _ left right) = max (expressionEvalStackSize left) (expressionEvalStackSize right) + 1
expressionEvalStackSize (UnaryExpression _ expr) = expressionEvalStackSize expr + 1