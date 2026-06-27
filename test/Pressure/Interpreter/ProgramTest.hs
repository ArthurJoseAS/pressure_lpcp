module Pressure.Interpreter.ProgramTest (programTests) where

import Pressure.Interpreter.Error qualified as Eval
import Pressure.Interpreter.Value (Value (..))
import Pressure.TestUtil
import Pressure.Typechecker.Error qualified as Type
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

programTests :: TestTree
programTests =
  testGroup
    "program"
    [ testCase "runs a valid program" testRunsValidProgram,
      testCase "runs program with main that computes" testRunsProgramWithComputation,
      testCase "propagates division by zero" testProgramDivisionByZero,
      testCase "propagates cast error" testProgramCastError,
      testCase "reports missing main" testMissingMain,
      testCase "reports invalid main signature" testInvalidMainSignature
    ]

testRunsValidProgram :: IO ()
testRunsValidProgram = do
  result <- evalProgramFromSource "main :: fn() {};"
  case result of
    Right VUnit -> return ()
    Right other -> error $ "expected VUnit, got " ++ show other
    Left err -> error $ "expected success, got " ++ show err

testRunsProgramWithComputation :: IO ()
testRunsProgramWithComputation = do
  result <- evalProgramFromSource "main :: fn() { x: int = 1 + 2; };"
  case result of
    Right VUnit -> return ()
    Right other -> error $ "expected VUnit, got " ++ show other
    Left err -> error $ "expected success, got " ++ show err

testProgramDivisionByZero :: IO ()
testProgramDivisionByZero = do
  result <- evalProgramFromSource "main :: fn() { x: int = 1 / 0; };"
  case result of
    Left (EvalError (Eval.RuntimeError (Eval.DivisionByZero _))) -> return ()
    Left err -> error $ "expected division by zero, got " ++ show err
    Right val -> error $ "expected error, got " ++ show val

testProgramCastError :: IO ()
testProgramCastError = do
  result <- evalProgramFromSource "main :: fn() { x: int = @as(int, \"not a number\"); };"
  case result of
    Left (EvalError (Eval.RuntimeError (Eval.CastError _ _))) -> return ()
    Left err -> error $ "expected cast error, got " ++ show err
    Right val -> error $ "expected error, got " ++ show val

testMissingMain :: IO ()
testMissingMain = do
  result <- evalProgramFromSource "x: int = 1;"
  case result of
    Left (TypeError Type.MissingMain) -> return ()
    Left err -> error $ "expected missing main, got " ++ show err
    Right val -> error $ "expected error, got " ++ show val

testInvalidMainSignature :: IO ()
testInvalidMainSignature = do
  result <- evalProgramFromSource "main :: fn(x: int) {};"
  case result of
    Left (TypeError (Type.InvalidMain _ _)) -> return ()
    Left err -> error $ "expected invalid main, got " ++ show err
    Right val -> error $ "expected error, got " ++ show val
