{-# LANGUAGE FlexibleInstances #-}

module VariadicAdd where

class SuperAdd t where
  plus :: Integer -> t

instance SuperAdd Integer where
  plus = id

instance (SuperAdd t) => SuperAdd (Integer -> t) where
  plus m n = plus (m + n)

