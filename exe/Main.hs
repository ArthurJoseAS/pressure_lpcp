module Main (main) where

import Pressure.Interpreter.Program (run)
import Pressure.Interpreter.Repl (repl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    file : _ -> run file
