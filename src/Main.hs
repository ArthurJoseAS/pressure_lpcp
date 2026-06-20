module Main (main) where

import Lexer (tokenize)
import Parser (parseProgram, parseRepl)
import System.Environment (getArgs)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    filename : _ -> do
      content <- readFile filename
      execProgram content
      return ()

repl :: IO ()
repl = do
  done <- isEOF
  if done
    then return ()
    else do
      line <- getLine
      eval line
      repl

eval :: String -> IO ()
eval = putStrLn . show . parseRepl . tokenize

execProgram :: String -> IO ()
execProgram = putStrLn . show . parseProgram . tokenize
