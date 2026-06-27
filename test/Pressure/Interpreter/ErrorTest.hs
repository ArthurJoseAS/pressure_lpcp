module Pressure.Interpreter.ErrorTest
  ( errorTests,
  )
where

import Pressure.Interpreter.Error qualified as Eval
import Pressure.Language.Lexer (AlexPosn (..))
import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

errorTests :: TestTree
errorTests =
  testGroup
    "errors"
    [ testCase "formats runtime errors" testRuntimeErrorMessageFormat
    ]

testRuntimeErrorMessageFormat :: IO ()
testRuntimeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (mPos, m) = Eval.errorInfo (Eval.RuntimeError (DivisionByZero pos))
  assertEqual "runtime div by zero pos" (Just pos) mPos
  assertEqual "runtime div by zero text" "division by zero" m
