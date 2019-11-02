module Main where

import System.Console.Haskeline
import Control.Monad (forM_, forever)

import Codewars.TinyThreePassCompiler
import Lib

mainLoop :: InputT IO ()
mainLoop = forever $ do
  line <- getInputLine "TinyCompiler> "
  case line of
    Nothing -> return ()
    Just ":q" -> return ()
    Just input -> do
      let ir = compileIR input
      (outputStrLn . show . pass2 . pass1) input
      forM_ ir (outputStrLn . show)
      (outputStrLn . ("runIR  = " ++) . show . vmR0) $ runIR ir [1..]
      (outputStrLn . ("runIR' = " ++) . show . vmR0) $ runIR' ir [1..]

main :: IO ()
main = runInputT defaultSettings mainLoop
