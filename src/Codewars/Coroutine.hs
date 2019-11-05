{-# LANGUAGE DeriveFunctor #-}

module Codewars.Coroutine where

import Control.Monad (ap, forever, when, replicateM_)
import Control.Applicative
import Prelude hiding(filter)

newtype Coroutine r u d a = Coroutine {
  runCoroutine :: (Command r u d a -> r) -> r
} deriving (Functor)

data Command r u d a =
    Done a
  | Out d (Coroutine r u d a)
  | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure  = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
    return x = Coroutine (\k -> k (Done x))
    f >>= g  = Coroutine (\k -> apply f (\co -> case co of
        Done a  -> apply (g a) k
        Out d c -> k (Out d (c >>= g))
        In c -> k (In (fmap (>>= g) c))))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine (\k -> apply p2 (\co -> case co of
    Done a -> k (Done a)
    Out d c -> k (Out d (p1 >>> c))
    In c -> apply (pipe2 p1 c) k))

-- It might be useful to define the following function

pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 p1 p2 = Coroutine (\k -> apply p1 (\co -> case co of
    Done a -> k (Done a)
    Out d c -> apply (c >>> p2 d) k
    In c -> k (In ((`pipe2` p2) <$> c))))

-- Library functions

-- | Waits for an input before returning it.
input :: Coroutine r v d v
input = Coroutine (\k -> k (In return))

-- | Immediately output the argument.
output :: a -> Coroutine r u a ()
output v = Coroutine (\k -> k (Out v (return ())))

-- | Output each element in a list in order.
produce :: [a] -> Coroutine r u a ()
produce =  mapM_ output

-- | Collect all outputted values into a list.
consume :: Coroutine [t] u t a -> [t]
consume x = apply x (\co -> case co of
    Out d c -> d : consume c
    _ -> [])

-- | Repeatedly request for input and output it, if it matches a predicate.
filter :: (v -> Bool) -> Coroutine r v v ()
filter p = forever (input >>= \v -> when (p v) (output v))

-- | Allow n items to pass through before terminating (similar to take from the prelude).
limit :: Int -> Coroutine r v v ()
limit n = replicateM_ n (input >>= output)

-- | Disallow the first n items to pass through (similar to drop from the prelude).
suppress :: Int -> Coroutine r v v ()
suppress n = replicateM_ n input >> forever (input >>= output)

-- | Repeatedly take two inputs and output their sum.
add :: Coroutine r Int Int ()
add = forever (input >>= \v1 -> input >>= \v2 -> output (v1 + v2))

-- | Repeatedly receive input and output it twice.
duplicate :: Coroutine r v v ()
duplicate = forever (input >>= \v -> output v >> output v)

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filter even >>> limit 5
p2 = produce [ x * (x+1) `div` 2 | x <- [1..] ]
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
