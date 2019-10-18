{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Prove(plusCommutes) where

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

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS e1) (EqlS e2) = EqlS $ transitive e1 e2

-- | a + (b ++) = (a + b) ++
lemma :: Natural m -> Natural n -> Equal (m :+: S n) (S (m :+: n))
lemma NumZ NumZ = EqlS EqlZ
lemma NumZ (NumS n) = EqlS $ lemma NumZ n
lemma (NumS m) n = EqlS $ lemma m n

-- | a + zero = zero + a
-- | a + (b ++) = (a + b) ++
-- | (b ++) + a = (b + a) ++
-- | then a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes (NumS m) NumZ = EqlS $ plusCommutes m NumZ
plusCommutes m (NumS n) = transitive (lemma m n) $ EqlS $ plusCommutes m n
