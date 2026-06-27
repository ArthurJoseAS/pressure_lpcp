module Pressure.Interpreter.ArithTest (arithTests) where

import Pressure.Interpreter.Error qualified as Eval
import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Ast
import Pressure.Language.Types
import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

arithTests :: TestTree
arithTests =
  testGroup
    "arithmetic"
    [ testCase "reports division by zero" testDivByZero,
      testCase "evaluates mixed subtraction" testMixedSubEval
    ]

testDivByZero :: IO ()
testDivByZero = do
  withTokens "division by zero int" "x: int = 1 / 0;" $ \ast -> do
    result <- evalParsed "division by zero int" ast
    case result of
      Left (Eval.RuntimeError (DivisionByZero _)) -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

  withTokens "division by zero float" "x: float = 1.0 / 0.0;" $ \ast -> do
    result <- evalParsed "division by zero float" ast
    case result of
      Left (Eval.RuntimeError (DivisionByZero _)) -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

testMixedSubEval :: IO ()
testMixedSubEval =
  assertExpr
    "float subtraction eval"
    ( TypedExpr
        pos0
        (FloatT F64)
        ( TypedBinaryExpr
            SubOp
            (TypedExpr pos0 (FloatT F64) (TypedFloatLit 8.5))
            (TypedExpr pos0 (FloatT F64) (TypedFloatLit 3.0))
        )
    )
    emptyEnv
    (VFloat F64 5.5)
