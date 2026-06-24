module Main (main) where

import Pressure.Interpreter.Repl (repl, run)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    filename : _ -> do
      program <- readFile filename
      run program
