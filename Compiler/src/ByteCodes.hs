module ByteCodes where

data ByteCode = LDCNST Int
    | STLOC Int
    | LDLOC Int
    | ADD
    | SUB
    | CMP
    | NOT
    | AND
    | OR
    | JMP Int
    | JMPNIF Int

byteCodesAsInts :: [ByteCode] -> [Int]
byteCodesAsInts [] = []
byteCodesAsInts (x:xs) = prepend (byteCodeAsInts x) (byteCodesAsInts xs)
    where
        prepend :: [Int] -> [Int] -> [Int]
        prepend [] ints = ints
        prepend (x:xs) ints = x : prepend xs ints

byteCodeAsInts :: ByteCode -> [Int]
byteCodeAsInts (LDCNST x) = [1, x]
byteCodeAsInts (STLOC x) = [2, x]
byteCodeAsInts (LDLOC x) = [3, x]
byteCodeAsInts ADD = [4]
byteCodeAsInts SUB = [5]
byteCodeAsInts CMP = [6]
byteCodeAsInts NOT = [7]
byteCodeAsInts AND = [8]
byteCodeAsInts OR = [9]
byteCodeAsInts (JMP x) = [10, x]
byteCodeAsInts (JMPNIF x) = [11, x]

