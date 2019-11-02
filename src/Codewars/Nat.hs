module Codewars.Nat where

data Nat = Zero | Succ Nat

natAdd :: Nat -> Nat -> Nat
natAdd Zero n = n
natAdd (Succ m) n = natAdd m (Succ n)


