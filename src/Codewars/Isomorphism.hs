module Codewars.Isomorphism where

import Data.Void
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

swap :: (a, b) -> (b, a)
swap = uncurry $ flip (,)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm = swap

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (fab, fba) (fbc, fcb) = (fbc . fab, fba . fcb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (fab, fba) (fcd, fdc) = (fac_bd, fbd_ac)
  where fac_bd (a, c) = (fab a, fcd c)
        fbd_ac (b, d) = (fba b, fdc d)

isoApplicative :: Applicative t => ISO a b -> ISO (t a) (t b)
isoApplicative (fab, fba) = ((<$>) fab, (<$>) fba)

isoList :: ISO a b -> ISO [a] [b]
isoList = isoApplicative

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe = isoApplicative

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (fab, fba) (fcd, fdc) = (fac_bd, fbd_ac)
  where fac_bd (Left a) = Left (fab a)
        fac_bd (Right c) = Right (fcd c)
        fbd_ac (Left b) = Left (fba b)
        fbd_ac (Right d) = Right (fdc d)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (fab, fba) (fcd, fdc) = (w1, w2)
  where w1 f = fcd . f . fba
        w2 f = fdc . f . fab

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (fab, fba) = (factory fab, factory fba)
  where factory f x = case f (Just x) of
                        Just y -> y
                        Nothing -> case f Nothing of
                                     Nothing -> error "f**k"
                                     Just y -> y
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g)
  where f (Right ()) = Left []
        f (Left x) = Left $ () : x
        g (Left []) = Right ()
        g (Left (_:xs)) = Left xs
        -- g (Right Void) is impossible
        
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (swap, swap)
-- where f (a, b) = (b, a)
