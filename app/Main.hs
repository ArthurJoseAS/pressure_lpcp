module Main where
import System.Environment
import Lexer (tokenize)
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
    res <- parser tokens
    case res of
      Left err -> print err
      Right (result, state) -> do
        mapM_ print result
        putStrLn "\nParser State:"
        print state