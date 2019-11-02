module Main where

import System.Console.Haskeline
import Control.Monad (forM_, forever)

import Codewars.TinyThreePassCompiler
import Lib

mainLoop :: InputT IO ()
mainLoop = forever $ do
  line <- getInputLine "> "
  case line of
    Nothing -> return ()
    Just ":q" -> return ()
    Just input -> do
      forM_ (compileIR input) (outputStrLn . show)

main :: IO ()
main = runInputT defaultSettings mainLoop
