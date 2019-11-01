{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module PolyvariadicFunction where

class AddResult r where
  polyAdd' :: Int -> r

instance AddResult Int where
  polyAdd' = id

instance (a ~ Int, AddResult r) => AddResult (a -> r) where
  polyAdd' x = polyAdd' . (x +)

polyAdd :: AddResult r => r
polyAdd = polyAdd' 0

-- `polyList` turns its arguments into a list, in a polymorphic way.
class ListResult a r | r -> a where
  polyList' :: ([a] -> [a]) -> r

instance ListResult a [a] where
  polyList' f = f []

instance (ListResult a r) => ListResult a (a -> r) where
  polyList' f x = polyList' (f . (:) x)

polyList :: (ListResult a r) => r
polyList = polyList' id

-- `polyWords` turns its arguments into a spaced string.
class WordsResult r where
  polyWords' :: String -> r

instance WordsResult String where
  polyWords' = id

instance (a ~ String, WordsResult r) => WordsResult (a -> r) where
  polyWords' x = polyWords' . (f x ++)
    where f x | null x = x
              | otherwise = x ++ " "
  
polyWords :: WordsResult r => r
polyWords = polyWords' []

