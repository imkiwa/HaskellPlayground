{-# LANGUAGE MultiWayIf #-}

module Codewars.TinyThreePassCompiler where

-- | The programming language has this syntax:
-- |
-- |   function   ::= '[' arg-list ']' expression
-- |
-- |   arg-list   ::= /* nothing */
-- |                | variable arg-list
-- |
-- |   expression ::= term
-- |                | expression '+' term
-- |                | expression '-' term
-- |
-- |   term       ::= factor
-- |                | term '*' factor
-- |                | term '/' factor
-- |
-- |   factor     ::= number
-- |                | variable
-- |                | '(' expression ')'
data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show, Read)

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
data IR = IMM Int
        | ARG Int
        | SWAP
        | PUSH
        | POP
        | ADD
        | SUB
        | MUL
        | DIV
        deriving (Eq, Read)

-- | According to the kate
instance Show IR where
  show (ARG i) = "AR " ++ show i
  show (IMM i) = "IM " ++ show i
  show SWAP    = "SW"
  show PUSH    = "PU"
  show POP     = "PO"
  show ADD     = "AD"
  show SUB     = "SU"
  show MUL     = "MU"
  show DIV     = "DI"


data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

type TreeCtor = AST -> AST -> AST
type Op = Int -> Int -> Int

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

compileIR :: String -> [IR]
compileIR = pass3IR . pass2 . pass1

pass1 :: String -> AST
pass1 = genTree . tokenize

pass2 :: AST -> AST
pass2 = optimize

pass3 :: AST -> [String]
pass3 = (map show) . pass3IR

pass3IR :: AST -> [IR]
pass3IR = codegen

-- | Phase 1:
-- | This phase will build an AST from tokens.
type ArgList = [String]
type ParseData = (AST, ArgList, [Token])

type ParseFunc = ([Token] -> ArgList -> ParseData)
type PartialParseFunc = [Token] -> ([AST -> AST], [Token])

-- | Helper parse factory for parsing ASTs like:
-- |   node       ::= factor
-- |                | node op1 factor
-- |                | node op2 factor
parsePrioritized :: [Token] -> ArgList -> [Token] -> ParseFunc -> ParseData
parsePrioritized tokens args ops parseFunc = (ast, args, restTokens)
  where (factor, _, r)          = parseFunc tokens args
        (nodeCtors, restTokens) = (doIfMatch ops parseFunc) r
        ast                     = foldl (flip id) factor nodeCtors

        -- | Do ParseFunc if current token matches one of the [Token]
        doIfMatch :: [Token] -> ParseFunc -> PartialParseFunc
        doIfMatch _ _ [] = ([], [])
        doIfMatch ops parseFunc (currentToken : rs)
              | currentToken `elem` ops   = (flip nodeCtor subNode : nodeCtors, restTokens)
              | otherwise                 = ([], currentToken : rs)
                 where nodeCtor                 = mapOp currentToken
                       (subNode, _, tokens)     = parseFunc rs args
                       (nodeCtors, restTokens)  = doIfMatch ops parseFunc tokens

parseExpr :: [Token] -> ArgList -> ParseData
parseExpr [] _ = error "Unexpected EOF"
parseExpr tokens args = parsePrioritized tokens args [TChar '+', TChar '-'] parseTerm

parseTerm :: [Token] -> ArgList -> ParseData
parseTerm tokens args = parsePrioritized tokens args [TChar '*', TChar '/'] parseFactor

parseFactor :: [Token] -> ArgList -> ParseData
parseFactor [] _ = error "Unexptected EOF"
parseFactor (TInt number : xs)   args = (Imm number, args, xs)
parseFactor (TStr variable : xs) args = (Arg (indexOf variable args), args, xs)
parseFactor (TChar '(' : xs)     args = (node, args, tail rest)
  where (node, _, rest) = parseExpr xs args

extractVarName :: Token -> String
extractVarName (TStr str) = str
extractVarName _ = error "Unexpected variable name"

genTree :: [Token] -> AST
genTree tokens = ast
  where (TChar '[' : paramTokens, TChar ']' : bodyTokens) = break (== TChar ']') tokens
        args = map extractVarName paramTokens
        (ast, _, _) = parseExpr bodyTokens args

mapOp :: Token -> AST -> AST -> AST
mapOp (TChar '+') = Add
mapOp (TChar '-') = Sub
mapOp (TChar '*') = Mul
mapOp (TChar '/') = Div

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = length (takeWhile (/= x) xs)

-- | Phase 2:
-- | This phase will take the output from genTree and return
-- | a new AST (with the same format) with all constant
-- | expressions reduced as much as possible.
optimize :: AST -> AST

-- | Imm and Arg don't need constant folding.
-- | They are already **constants**
optimize c@(Imm _) = c
optimize c@(Arg _) = c
-- | Foreach sub-AST, recursively call foldConstants on the result of `pass2 sub-AST`
optimize (Add astL astR) = foldConstants Add (+) (optimize astL) (optimize astR)
optimize (Sub astL astR) = foldConstants Sub (-) (optimize astL) (optimize astR)
optimize (Mul astL astR) = foldConstants Mul (*) (optimize astL) (optimize astR)
optimize (Div astL astR) = foldConstants Div div (optimize astL) (optimize astR)

-- | Fold constants to reduce tree size
foldConstants :: TreeCtor -> Op -> AST -> AST -> AST

-- | Fold two Imm into an Imm if both sides are Imm
foldConstants _ op (Imm lhs) (Imm rhs) = Imm (op lhs rhs)

-- | Fold (0 op *) and (* op 0)
foldConstants ctor _ i@(Imm 0) ast = let test = ctor i ast
                                      in case test of
                                          (Add _ _) -> ast
                                          (Mul _ _) -> Imm 0
                                          (Div _ _) -> Imm 0
                                          otherwise -> test
foldConstants ctor _ ast i@(Imm 0) = let test = ctor ast i
                                      in case test of
                                          (Add _ _) -> ast
                                          (Mul _ _) -> Imm 0
                                          otherwise -> test

-- | Fold (x - x) to 0
foldConstants ctor _ l@(Arg lhs) r@(Arg rhs)
  | lhs == rhs = let test = ctor l r
                 in case test of
                      (Sub _ _) -> Imm 0
                      otherwise -> test
  | otherwise  = ctor l r

-- | Otherwise, do nothing
foldConstants ctor _ astL astR = ctor astL astR

-- | Record my stupid mistake!!!
--foldConstants' :: Op -> AST -> AST -> AST
--foldConstants' op (Imm lhs) (Imm rhs) = Imm (op lhs rhs)
--foldConstants' op astL astR = ctor astL astR
--  where ctor = if | op == (+) -> Add
--                  | op == (-) -> Sub
--                  | op == (*) -> Mul
--                  | otherwise -> Div

-- | Phase 3:
-- | You are working on a small processor with two registers (R0 and R1),
-- | a stack, and an array of input arguments.
-- | The result of a function is expected to be in R0.
-- |
-- | Register flow notion:
-- |   x -> r     // write x to r
-- |   r -> x     // write r to x
-- |   r1 <-> r2  // swap r1 r2
codegen :: AST -> [IR]

-- | * -> R0
codegen (Imm x) = [IMM x]
codegen (Arg n) = [ARG n]

-- | R0 (op) R1 -> R0
codegen (Add astL astR) = genCommutableIR ADD astL astR
codegen (Sub astL astR) = genIR SUB astL astR
codegen (Mul astL astR) = genCommutableIR MUL astL astR
codegen (Div astL astR) = genIR DIV astL astR

-- | Calculate astL (op) astR
-- |
-- | codegen astL -> R0
-- |           R0 -> stack
-- | codegen astR -> R0
-- |           R0 -> R1
-- |        stack -> R0
-- | call op
genIR :: IR -> AST -> AST -> [IR]
genIR ir astL astR = concat [codeAstL, [PUSH], codeAstR, [SWAP], [POP], [ir]]
  where gen = codegen
        codeAstL = gen astL
        codeAstR = gen astR

-- | Generate code for operand-order-insensitive instructions
genCommutableIR :: IR -> AST -> AST -> [IR]
genCommutableIR ir astL astR = concat [codeAstL, [SWAP], codeAstR, [ir]]
  where gen = codegen
        codeAstL = gen astL
        codeAstR = gen astR


-- | The virtual machine
type R0 = Int
type R1 = Int

data VM = VM {
  vmR0      :: R0,
  vmR1      :: R1,
  vmStack   :: [Int],
  vmIR      :: [IR]
}

createVM :: VM
createVM = VM {vmR0 = 0, vmR1 = 0, vmStack = [], vmIR = []}

runIR :: [IR] -> VM
runIR ir = vmRunLoop (createVM {vmIR = ir})

vmRunLoop :: VM -> VM
vmRunLoop vm = undefined

