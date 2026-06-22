module Ast.ErrorTest
  ( testBoolInArithmeticError,
    testBoolInArithmeticRightError,
    testTypeMismatchError,
    testFloatNarrowingError,
    testUndefinedVariableTypeError,
    testMissingAnnotationError,
    testDuplicateParamsRejected,
    testDuplicateFunctionsRejected,
    testDuplicateDeclarationsRejected,
    testTypeErrorMessageFormat,
    testRuntimeErrorMessageFormat,
  )
where

import Ast hiding (Error, UndefinedVariable)
import Ast.Typecheck qualified as T
import Eval qualified as Eval
import Lexer (AlexPosn (..))
import TestUtil

testBoolInArithmeticError :: IO ()
testBoolInArithmeticError = checkErr "bool in arithmetic" "x: int = true + 1;"

testBoolInArithmeticRightError :: IO ()
testBoolInArithmeticRightError = checkErr "bool on right of arithmetic" "x: int = 1 + true;"

testTypeMismatchError :: IO ()
testTypeMismatchError = checkErr "type mismatch" "x: bool = 42;"

testFloatNarrowingError :: IO ()
testFloatNarrowingError = checkErr "float to int narrowing" "x: int = 3.14;"

testUndefinedVariableTypeError :: IO ()
testUndefinedVariableTypeError = checkErr "undefined variable" "x: int = y;"

testMissingAnnotationError :: IO ()
testMissingAnnotationError =
  case checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") Nothing Nothing)))]) of
    Left _ -> return ()
    Right () -> error "missing annotation: expected type error but passed"

testDuplicateParamsRejected :: IO ()
testDuplicateParamsRejected = do
  checkErr "duplicate params" "f :: fn(x: i32, x: i32) -> i32 { x };"

testDuplicateFunctionsRejected :: IO ()
testDuplicateFunctionsRejected = do
  checkErr "duplicate functions" "f :: fn() -> i32 { 1 }; f :: fn() -> i32 { 2 };"

testDuplicateDeclarationsRejected :: IO ()
testDuplicateDeclarationsRejected = do
  checkErr "duplicate declarations" "x :: 1; x :: 2;"

testTypeErrorMessageFormat :: IO ()
testTypeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (p, m) = T.errorInfo (T.TypeMismatch pos (IntType (AlexPn 0 1 1) Signed I32) (BoolType (AlexPn 0 1 1)))
  assertEqual "type mismatch pos" pos p
  assertEqual "type mismatch text" "type mismatch: expected 'i32', found 'bool'" m

  let (p2, m2) = T.errorInfo (T.UnsupportedOp pos AddOp (IntType (AlexPn 0 1 1) Signed I32) (BoolType (AlexPn 0 1 5)))
  assertEqual "unsupported op pos" pos p2
  assertEqual "unsupported op text" "cannot use operator '+' on type 'i32' and 'bool'" m2

  let (p3, m3) = T.errorInfo (T.UndefinedVariable pos "foo")
  assertEqual "type undefined pos" pos p3
  assertEqual "type undefined text" "undefined variable 'foo'" m3

testRuntimeErrorMessageFormat :: IO ()
testRuntimeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (mPos, m) = Eval.errorInfo (RuntimeError (DivisionByZero pos))
  assertEqual "runtime div by zero pos" (Just pos) mPos
  assertEqual "runtime div by zero text" "division by zero" m
