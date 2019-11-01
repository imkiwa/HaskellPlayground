module Main where

import Lib

insert :: Eq a => a -> [a] -> [[a]]
insert n [] = [[n]]
insert n (nh : nt) = (n : nh : nt) : [nh : nt' | nt' <- insert n nt]

permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = concat [insert x rest | rest <- permutations xs]

delete :: Eq a => a -> [a] -> [a]
delete x xs = [ y | y <- xs, y /= x ]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x : r | x <- xs, r <- permutations (delete x xs)]

main :: IO ()
main = putStrLn $ show $ permutations "aabb"
