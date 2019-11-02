{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Codewars.ProvePlusAssoc(plusAssoc) where

-- | The natural numbers.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS n) = EqlS $ symmetric n

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS m) (EqlS n) = EqlS $ transitive m n

-- The plus
-- | 0 + b = b
-- | (a + 1) + b = (a + b) + 1
plus :: Natural a -> Natural b -> Natural (a :+: b)
plus NumZ b = b
plus (NumS a) b = NumS $ plus a b

-- This is the proof that the kata requires.
-- | (0 + a) + b = 0 + (a + b)
-- | (a + 1) + b = (a + b) + 1
-- | a + (b + 1) = (a + b) + 1
-- | => (a + b) + c = a + (b + c)
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal ((a :+: b) :+: c) (a :+: (b :+: c))
plusAssoc NumZ a b = reflexive $ plus a b
plusAssoc (NumS n) a b = EqlS $ plusAssoc n a b
