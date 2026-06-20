module Main (main) where

import AstTest (testAst)
import LexerTest (testLexer)
import ParserTest (testParser)

main :: IO ()
main = do
  putStrLn "Running lexer tests..."
  testLexer
  putStrLn "Running parser tests..."
  testParser
  putStrLn "Running AST tests..."
  testAst
  putStrLn "All tests passed."
