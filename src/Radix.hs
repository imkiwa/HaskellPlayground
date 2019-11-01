module Radix where

type Radix = Int

digits :: Int -> [Int]
digits = map (read . (:[])) . show

toDecimal :: [Int] -> Radix -> [Int]
toDecimal []     n = []
toDecimal (x:xs) n = (x * n ^ (length xs)) : toDecimal xs n

decimalFrom :: Int -> Radix -> Int
decimalFrom a n = sum $ toDecimal (digits a) n

toRadix :: Int -> Radix -> String
toRadix 0 n = []
toRadix a n = toRadix (a `div` n) n ++ show (a `mod` n) 

radixTo :: (Int, Radix) -> Radix -> Int
radixTo (a, n) m = read $ (a `decimalFrom` n) `toRadix` m

