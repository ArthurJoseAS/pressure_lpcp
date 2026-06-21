module LexerTest (testLexer) where

import Lexer (AlexPosn (..), Token (..), tokenizeEither)

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
      tokenizeEither "( ) { } [ ] . , ; : ' \""
  assertEqual "delimiter count" 12 (length parens)

  ids <- assertRight "tokenize identifiers" $ tokenizeEither "foo bar_baz x'"
  assertEqual "identifier count" 3 (length ids)

  nums <- assertRight "tokenize numbers" $ tokenizeEither "42 3.14"
  assertEqual "number count" 2 (length nums)

  let input = "let x: i32 = 42;"
  _ <- assertRight "tokenize declaration" $ tokenizeEither input

  eq <- assertRight "tokenize equality operator" $ tokenizeEither "=="
  case eq of
    [CmpEq _] -> return ()
    other -> error $ "expected CmpEq, got " ++ show other

  comment <- assertRight "skip line comment" $ tokenizeEither "x // ignored\ny"
  case comment of
    [Id _ "x", Id _ "y"] -> return ()
    other -> error $ "expected identifiers around skipped comment, got " ++ show other

  positioned <- assertRight "token positions" $ tokenizeEither "x\n  y"
  case positioned of
    [Id (AlexPn _ 1 1) "x", Id (AlexPn _ 2 3) "y"] -> return ()
    other -> error $ "unexpected token positions: " ++ show other

  assertLeft "invalid character" $ tokenizeEither "@"
  return ()

assertLeft :: String -> Either String a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected lexer error"
