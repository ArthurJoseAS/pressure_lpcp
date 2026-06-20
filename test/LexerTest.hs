module LexerTest (testLexer) where

import Lexer (tokenizeEither)

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then return ()
    else error $ name ++ " failed:\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual

assertRight :: String -> Either String a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ err
assertRight _ (Right x) = return x

testLexer :: IO ()
testLexer = do
  tokens <-
    assertRight "tokenize keywords" $
      tokenizeEither "if else true false for continue break fn struct enum return"
  assertEqual "keyword count" 11 (length tokens)

  ops <-
    assertRight "tokenize operators" $
      tokenizeEither "= < > == != <= >= -> and or ! + - >> << * / &"
  assertEqual "operator count" 18 (length ops)

  parens <-
    assertRight "tokenize delimiters" $
      tokenizeEither "( ) { } [ ] :: . , ; : ' \""
  assertEqual "delimiter count" 14 (length parens)

  ids <- assertRight "tokenize identifiers" $ tokenizeEither "foo bar_baz x'"
  assertEqual "identifier count" 3 (length ids)

  nums <- assertRight "tokenize numbers" $ tokenizeEither "42 3.14"
  assertEqual "number count" 2 (length nums)

  let input = "let x: i32 = 42;"
  _ <- assertRight "tokenize declaration" $ tokenizeEither input
  return ()
