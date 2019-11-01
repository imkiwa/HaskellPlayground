module ApproCalculation where

import Control.Applicative

fact :: (Eq a, Num a) => a -> a
fact 1 = 1
fact n = n * fact (n - 1)

lnList x = getZipList $ (\x y z -> x * y * z) <$> a <*> b <*> c
    where a = ZipList $ zipWith (^) (repeat (-1)) [0..]
          b = ZipList $ zipWith (/) (repeat 1) [1..]
          c = ZipList $ zipWith (^) (repeat (x - 1)) [1..]

ln x n = sum $ take n $ lnList x

e :: Double
e = 1 + sum (takeWhile (>= 1.0e-19) $ map ((1 /) . fromIntegral . fact) [1 ..])

