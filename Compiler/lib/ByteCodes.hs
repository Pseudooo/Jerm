module ByteCodes where

import Expressions

data ByteCode = LDCNST Int
    | STLOC Int
    | LDLOC Int
    | ADD
    | SUB
    | MUL
    | DIV
    | CMP
    | NOT
    | AND
    | OR
    | JMP Int
    | JMPNIF Int

byteCodesAsInts :: [ByteCode] -> [Int]
byteCodesAsInts [] = []
byteCodesAsInts (bc:bcs) = prepend (byteCodeAsInts bc) (byteCodesAsInts bcs)
    where
        prepend :: [Int] -> [Int] -> [Int]
        prepend [] ints = ints
        prepend (x:xs) ints = x : prepend xs ints

byteCodesSize :: [ByteCode] -> Int
byteCodesSize [] = 0
byteCodesSize (x:xs) = (length . byteCodeAsInts $ x) + byteCodesSize xs

byteCodeAsInts :: ByteCode -> [Int]
byteCodeAsInts (LDCNST x) = [1, x]
byteCodeAsInts (STLOC x) = [2, x]
byteCodeAsInts (LDLOC x) = [3, x]
byteCodeAsInts ADD = [4]
byteCodeAsInts SUB = [5]
byteCodeAsInts MUL = [6]
byteCodeAsInts DIV = [7]
byteCodeAsInts CMP = [8]
byteCodeAsInts NOT = [9]
byteCodeAsInts AND = [10]
byteCodeAsInts OR = [11]
byteCodeAsInts (JMP x) = [12, x]
byteCodeAsInts (JMPNIF x) = [13, x]

operatorByteCode :: Operator -> ByteCode
operatorByteCode OpAdd = ADD
operatorByteCode OpSub = SUB
operatorByteCode OpMul = MUL
operatorByteCode OpDiv = DIV
operatorByteCode OpEquals = CMP
operatorByteCode OpNot = NOT
operatorByteCode OpAnd = AND
operatorByteCode OpOr = OR
