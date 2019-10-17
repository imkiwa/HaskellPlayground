module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

at :: [a] -> Int -> a
at x i = (head . drop i) x
