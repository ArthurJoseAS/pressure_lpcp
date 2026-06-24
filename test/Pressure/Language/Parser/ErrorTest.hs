module Pressure.Language.Parser.ErrorTest (parserErrorTests) where

import Pressure.Language.Lexer (AlexPosn (..), runAlex)
import Pressure.Language.Parser (parseErrorInfo, parseProgram)
import Pressure.TestUtil (assertEqual, assertLeft, assertRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

parserErrorTests :: TestTree
parserErrorTests =
  testGroup
    "errors"
    [ testCase "rejects parse errors" testParseErrors,
      testCase "formats parse errors" testParseErrorFormat
    ]

testParseErrors :: IO ()
testParseErrors = do
  assertLeft "program requires semicolon" $ runAlex "1 + 2" parseProgram
  assertLeft "malformed expression" $ runAlex "x: int = 1 + ;" parseProgram
  assertLeft "chained comparisons are forbidden" $ runAlex "1 < 2 < 3;" parseProgram
  _ <- assertRight "if expression without else parses" $ runAlex "x: int = if true { 1 };" parseProgram
  return ()

testParseErrorFormat :: IO ()
testParseErrorFormat = do
  let (mPos, msg) = parseErrorInfo "1:10: unexpected identifier 'foo'"
  case mPos of
    Just (AlexPn _ line col) -> do
      assertEqual "parser error line" 1 line
      assertEqual "parser error col" 10 col
    Nothing -> error "expected position"
  assertEqual "parser error msg" "unexpected identifier 'foo'" msg
