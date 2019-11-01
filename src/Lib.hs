module Lib(
  say
  ) where

import           Data.List (group)
import Control.Monad (forM_)

say :: Show a => [a] -> IO ()
say = flip forM_ (putStrLn . show)
