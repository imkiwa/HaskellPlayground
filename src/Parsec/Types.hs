module Parsec.Types where

type Operand = Double
type Identifier = String 
type Variable = (Identifier, Operand)
type Context = [Variable]
