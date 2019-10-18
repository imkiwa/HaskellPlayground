module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

at :: [a] -> Int -> a
at x i = (head . drop i) x

iterate' :: (a -> a) -> a -> [a]
iterate' f i = i : iterate' f (f i)

