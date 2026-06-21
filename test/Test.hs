module Main (main) where

import AstTest (testAst)
import LexerTest (testLexer)
import ParserTest (testParser)
import TypeTest (testType)

main :: IO ()
main = do
  putStrLn "Running lexer tests..."
  testLexer
  putStrLn "Running parser tests..."
  testParser
  putStrLn "Running type tests..."
  testType
  putStrLn "Running AST tests..."
  testAst
  putStrLn "All tests passed."
