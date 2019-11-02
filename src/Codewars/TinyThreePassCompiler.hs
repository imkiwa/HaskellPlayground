module Codewars.TinyThreePassCompiler where

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show, Read)

data IR = IM Int
        | AR Int
        | SW
        | PU
        | PO
        | AD
        | SU
        | MU
        | DI
        deriving (Eq, Show, Read)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = parseTokens . tokenize

pass2 :: AST -> AST
pass2 = optimize

pass3 :: AST -> [String]
pass3 ast = map show $ codegen ast

parseTokens :: [Token] -> AST
parseTokens = undefined

-- | Phase 2:
-- | This phase will take the output from genTree and return 
-- | a new AST (with the same format) with all constant 
-- | expressions reduced as much as possible.
optimize :: AST -> AST

type TreeCtor = AST -> AST -> AST
type Op = (Int -> Int -> Int)

-- | Imm and Arg don't need constant folding.
-- | They are already **constants**
optimize c@(Imm _) = c
optimize c@(Arg _) = c
-- | Foreach sub-AST, recursively call foldConstants on the result of `pass2 sub-AST`
optimize (Add astL astR) = foldConstants Add (+) (pass2 astL) (pass2 astR)
optimize (Sub astL astR) = foldConstants Sub (-) (pass2 astL) (pass2 astR)
optimize (Mul astL astR) = foldConstants Mul (*) (pass2 astL) (pass2 astR)
optimize (Div astL astR) = foldConstants Div div (pass2 astL) (pass2 astR)

foldConstants :: TreeCtor -> Op -> AST -> AST -> AST
-- | Fold two Imm into an Imm if both sides are Imm
foldConstants _ op (Imm lhs) (Imm rhs) = Imm (op lhs rhs)
-- | Otherwise, do nothing
foldConstants ctor _ astL astR = ctor astL astR

-- | Record my stupid mistake!!!
--foldConstants :: Op -> AST -> AST -> AST
--foldConstants op (Imm lhs) (Imm rhs) = Imm (op lhs rhs)
--foldConstants op astL astR = ctor astL astR
--  where ctor = if | op == (+) -> Add
--                  | op == (-) -> Sub
--                  | op == (*) -> Mul
--                  | otherwise -> Div

-- | Phase 3:
-- | You are working on a small processor with two registers (R0 and R1),
-- | a stack, and an array of input arguments.
-- | The result of a function is expected to be in R0.
-- | The processor supports the following instructions:
-- |
-- |   "IM n"     // load the constant value n into R0
-- |   "AR n"     // load the n-th input argument into R0
-- |   "SW"       // swap R0 and R1
-- |   "PU"       // push R0 onto the stack
-- |   "PO"       // pop the top value off of the stack into R0
-- |   "AD"       // add R1 to R0 and put the result in R0
-- |   "SU"       // subtract R1 from R0 and put the result in R0
-- |   "MU"       // multiply R0 by R1 and put the result in R0
-- |   "DI"       // divide R0 by R1 and put the result in R0
-- |
-- | Register flow notion:
-- |   x -> r     // write x to r
-- |   r -> x     // write r to x
-- |   r1 <-> r2  // swap r1 r2
codegen :: AST -> [IR]

-- | -> R0
codegen (Imm x) = [IM x]
codegen (Arg n) = [AR n]

-- | Calculate astL (op) astR
-- |
-- | pass3 astL -> R0
-- |         R0 -> stack
-- | pass3 astR -> R0
-- |         R0 -> R1
-- |      stack -> R0
-- | call op

-- | -> R0
codegen (Add astL astR) = foldl (++) [] [(codegen astL), [PU], (codegen astR), [SW, PO], [AD]]
codegen (Sub astL astR) = foldl (++) [] [(codegen astL), [PU], (codegen astR), [SW, PO], [SU]]
codegen (Mul astL astR) = foldl (++) [] [(codegen astL), [PU], (codegen astR), [SW, PO], [MU]]
codegen (Div astL astR) = foldl (++) [] [(codegen astL), [PU], (codegen astR), [SW, PO], [DI]]

