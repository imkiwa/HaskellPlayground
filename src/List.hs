module List where

import           Data.List (group)

fib = 0:1:zipWith (+) fib (tail fib)

at :: [a] -> Int -> a
at x i = (head . drop i) x

iterate' :: (a -> a) -> a -> [a]
iterate' f i = i : iterate' f (f i)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' t (x:xs) =
  let rest = delete' t xs
   in if t == x
        then rest
        else x : rest

mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort (x:xs) (y:ys)
  | x > y = y : mergeSort (x : xs) ys
  | otherwise = x : mergeSort xs (y : ys)

mergeSort3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSort3 x y = mergeSort (mergeSort x y)

uniq :: Eq a => [a] -> [a]
uniq = map head . group

ham :: (Num a, Ord a) => [a]
ham = 1 : uniq (mergeSort3 (map (* 2) ham) (map (* 3) ham) (map (* 5) ham))

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

