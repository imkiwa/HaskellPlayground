module Parsec.Calculator where

import Parsec.Types
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.String

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

data Expression = Plus Expression Expression
                | Minus Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                | Negative Expression
                | Positive Expression
                | ID Identifier
                | Number Operand
                | Declaration Variable Expression
                deriving (Show, Eq)

type ContextStorage = Reader Context (Maybe Expression)

{-
 - Helper functions
 -}
chainRun :: Maybe Expression
         -> (Expression -> Maybe Expression)
         -> Maybe Expression
chainRun step f = case step of
                    Nothing -> Nothing
                    Just x -> f x

{-
 - Variables
 -}
-- TODO: name conflicts
putVar :: Variable -> Context -> Context
putVar = (:)

resolve :: Expression -> ContextStorage
resolve (Number n) = return (Just (Number n))
resolve (ID name) = do
  ctx <- ask
  case lookup name ctx of
    Nothing -> return Nothing
    Just v -> return (Just (Number v))
resolve (Negative e) = do
  res <- resolve e
  case res of
    Nothing -> return Nothing
    Just a -> return (Just (Negative a))
resolve (Positive e) = do
  res <- resolve e
  case res of
    Nothing -> return Nothing
    Just a -> return (Just (Positive a))
resolve (Plus l r) = do
  lres <- resolve l
  case lres of
    Nothing -> return Nothing
    Just a -> do
      rres <- resolve r
      case rres of
        Nothing -> return Nothing
        Just b -> return (Just (Plus a b))
resolve (Minus l r) = do
  lres <- resolve l
  case lres of
    Nothing -> return Nothing
    Just a -> do
      rres <- resolve r
      case rres of
        Nothing -> return Nothing
        Just b -> return (Just (Minus a b))
resolve (Multiply l r) = do
  lres <- resolve l
  case lres of
    Nothing -> return Nothing
    Just a -> do
      rres <- resolve r
      case rres of
        Nothing -> return Nothing
        Just b -> return (Just (Multiply a b))
resolve (Divide l r) = do
  lres <- resolve l
  case lres of
    Nothing -> return Nothing
    Just a -> do
      rres <- resolve r
      case rres of
        Nothing -> return Nothing
        Just b -> return (Just (Divide a b))                        
resolve (Declaration name exp) = local (putVar name) (resolve exp)

{-
 - Evaluation
 -}
eval :: Expression -> Operand
eval (Number n)         = n
eval (Plus lhs rhs)     = eval lhs + eval rhs
eval (Minus lhs rhs)    = eval lhs - eval rhs
eval (Multiply lhs rhs) = eval lhs * eval rhs
eval (Divide lhs rhs)   = eval lhs / eval rhs
eval (Negative e)       = negate $ eval e
eval (Positive e)       = eval e

calculate :: String -> Either ParseError Operand
calculate exp = fmap eval $ parse parseExpression "" exp

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

constant :: Parsec String () Operand
constant = choice [ lexeme $ string "pi" >> return pi,
                    lexeme $ string "e"  >> return 2.7182836 ]

decimal :: Parsec String () Operand
decimal = do
  n <- T.float lexer
  return n

number :: Parsec String () Operand
number = try (do
    int <- T.integer lexer
    return (fromIntegral int))
  <|>
    try decimal
  <|>
    constant

parseExpression :: Parser Expression
parseExpression = do
  e1 <- parseMultiply
  e2 <- parseExpression'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

parseExpression' :: Parser (Maybe (Expression -> Expression))
parseExpression' = do
    (lexeme.char) '+'
    e1 <- parseMultiply
    e2 <- parseExpression'
    case e2 of
      Nothing -> return (Just (\e -> Plus e e1))
      Just e -> return (Just (\e' -> e (Plus e' e1)))
  <|> do
    (lexeme.char) '-'
    e1 <- parseMultiply
    e2 <- parseExpression'
    case e2 of
       Nothing -> return (Just (\e  -> Minus e e1))
       Just e  -> return (Just (\e' -> e (Minus e' e1)))
  <|>
    return Nothing

parseMultiply :: Parser Expression
parseMultiply = do
  e1 <- parseUnaryExpression
  e2 <- parseMultiply'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

parseMultiply' :: Parsec String () (Maybe (Expression -> Expression))
parseMultiply' =
  do
    (lexeme.char) '*'
    e1 <- parseUnaryExpression
    e2 <- parseMultiply'
    case e2 of
      Nothing -> return (Just (\e -> Multiply e e1))
      Just e -> return (Just (\e' -> e (Multiply e' e1)))
  <|> do
    (lexeme.char) '/'
    e1 <- parseUnaryExpression
    e2 <- parseMultiply'
    case e2 of
      Nothing -> return (Just (\e -> Divide e e1))
      Just e -> return (Just (\e' -> e (Divide e' e1)))
  <|>
    return Nothing

parseUnaryExpression :: Parser Expression
parseUnaryExpression = do
  op <- choice (map (try.lexeme.string) ["-", "+"])
  e1 <- parseUnaryExpression
  case op of
    "+" -> return $ Positive e1
    "-" -> return $ Negative e1
    _ -> fail "unexpected operator"
  <|>
  parseNumber

parseNumber :: Parser Expression
parseNumber = try (do
    lexeme $ char '('
    e1 <- parseExpression
    lexeme $ char ')'
    return e1)
  <|> do
    num <- number
    return (Number num)


    
