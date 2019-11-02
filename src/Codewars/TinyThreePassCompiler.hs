module Codewars.TinyThreePassCompiler where

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
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

parseTokens :: [Token] -> AST
parseTokens = undefined

pass2 :: AST -> AST
-- | Imm and Arg don't need constant folding.
-- | They are already **constants**
pass2 c@(Imm _) = c
pass2 c@(Arg _) = c
-- | Foreach sub-AST, recursively call foldConstants on the result of `pass2 sub-AST`
pass2 (Add astL astR) = foldConstants Add (+) (pass2 astL) (pass2 astR)
pass2 (Sub astL astR) = foldConstants Sub (-) (pass2 astL) (pass2 astR)
pass2 (Mul astL astR) = foldConstants Mul (*) (pass2 astL) (pass2 astR)
pass2 (Div astL astR) = foldConstants Div div (pass2 astL) (pass2 astR)

type TreeCtor = AST -> AST -> AST
type Op = (Int -> Int -> Int)

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

pass3 :: AST -> [String]
pass3 = undefined

