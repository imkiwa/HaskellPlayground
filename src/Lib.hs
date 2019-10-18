module Lib where

import           Data.List (group)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

fact :: (Eq a, Num a) => a -> a
fact 1 = 1
fact n = n * fact (n - 1)

e :: Double
e = 1 + sum (takeWhile (>= 1.0e-19) $ map ((1 /) . fromIntegral . fact) [1 ..])
