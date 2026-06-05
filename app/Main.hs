module Main where
import System.Environment
-- import qualified Parser (someFunc)
import Lexer (tokenize, token_posn)
import Parser

main :: IO ()
main = do
  args <- getArgs
  if null args
    then print "No file provided"
  else do
    let filename = head args
    content <- readFile filename
    let tokens = tokenize content 
    case parser tokens of
      Left err -> print err
      Right result -> mapM_ print result