{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Codewars.ProvePlusCommutes(plusCommutes) where

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

-- | (a + b) + 1 = a + (b + 1)
helper :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
helper NumZ n = EqlS (reflexive n)
helper (NumS m) n = EqlS (helper m n)

-- This is the proof that the kata requires.
-- | a + 0 = 0 + a
-- | a + (b + 1) = (a + b) + 1
-- | (b + 1) + a = (b + a) + 1
-- | => a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS n) = EqlS $ plusCommutes NumZ n
plusCommutes (NumS m) n = transitive (EqlS $ plusCommutes m n) (helper n m)
