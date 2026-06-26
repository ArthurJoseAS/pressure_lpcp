module Pressure.Typechecker.ErrorTest (errorTypeTests) where

import Pressure.Language.Ast
import Pressure.Language.Lexer (AlexPosn (..), runAlex)
import Pressure.Language.Parser (parseProgram)
import Pressure.Language.Types
import Pressure.TestUtil (assertEqual, assertLeft, assertOk, checkErr, identFrom, pos0)
import Pressure.Typechecker (Error, checkProgram, checkRepl, checkReplWithEnv)
import Pressure.Typechecker.Error (errorInfo)
import Pressure.Typechecker.Error qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

errorTypeTests :: TestTree
errorTypeTests =
  testGroup
    "errors"
    [ testCase "rejects bool on left of arithmetic" testBoolInArithmeticError,
      testCase "rejects bool on right of arithmetic" testBoolInArithmeticRightError,
      testCase "rejects type mismatches" testTypeMismatchError,
      testCase "rejects float narrowing" testFloatNarrowingError,
      testCase "rejects undefined variables" testUndefinedVariableTypeError,
      testCase "rejects duplicate parameters" testDuplicateParamsRejected,
      testCase "rejects duplicate functions" testDuplicateFunctionsRejected,
      testCase "rejects duplicate declarations" testDuplicateDeclarationsRejected,
      testCase "rejects undefined types" testUndefinedTypeError,
      testCase "rejects break outside loop" testBreakOutsideLoop,
      testCase "rejects continue outside loop" testContinueOutsideLoop,
      testCase "rejects non-unit loop body" testNonUnitLoopBody,
      testCase "rejects break value type mismatch" testBreakValueMismatch,
      testCase "formats type errors" testTypeErrorMessageFormat,
      testCase "checks top-level types" testTopLevelTypes,
      testCase "checks repl types" testReplTypes
    ]

intType :: Type
intType = IntT Signed I32

intSyntax :: TypeSyntax
intSyntax = TypeSyntax pos0 (IntSyntax Signed I32)

expr :: ParsedExprKind -> ParsedExpr
expr = ParsedExpr pos0

checkSource :: String -> Either Error ()
checkSource source = case runAlex source parseProgram of
  Left err -> error $ "parse failed: " ++ err
  Right ast -> checkProgram ast

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

testDuplicateParamsRejected :: IO ()
testDuplicateParamsRejected = checkErr "duplicate params" "f :: fn(x: i32, x: i32) -> i32 { x };"

testDuplicateFunctionsRejected :: IO ()
testDuplicateFunctionsRejected = checkErr "duplicate functions" "f :: fn() -> i32 { 1 }; f :: fn() -> i32 { 2 };"

testDuplicateDeclarationsRejected :: IO ()
testDuplicateDeclarationsRejected = checkErr "duplicate declarations" "x :: 1; x :: 2;"

testUndefinedTypeError :: IO ()
testUndefinedTypeError = do
  checkErr "undefined type in function return" "foo :: fn() -> bar {};"
  checkErr "undefined type in annotation" "x: baz = 1;"
  checkErr "undefined type in function param" "f :: fn(x: qux) -> i32 { 1 };"
  checkErr "undefined type in function item return" "f :: fn() -> baz {};"

testBreakOutsideLoop :: IO ()
testBreakOutsideLoop = checkErr "break outside loop" "break;"

testContinueOutsideLoop :: IO ()
testContinueOutsideLoop = checkErr "continue outside loop" "continue;"

testNonUnitLoopBody :: IO ()
testNonUnitLoopBody = checkErr "non-unit loop body" "x: i32 = while true { 42 } else { 0 };"

testBreakValueMismatch :: IO ()
testBreakValueMismatch = checkErr "break value mismatch" "x: i32 = while true { break 1; } else { true };"

testTypeErrorMessageFormat :: IO ()
testTypeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (p, m) = errorInfo (T.TypeMismatch pos (IntT Signed I32) BoolT)
  assertEqual "type mismatch pos" pos p
  assertEqual "type mismatch text" "type mismatch: expected 'i32', found 'bool'" m

  let (p2, m2) = errorInfo (T.UnsupportedOp pos AddOp (IntT Signed I32) BoolT)
  assertEqual "unsupported op pos" pos p2
  assertEqual "unsupported op text" "cannot use operator '+' on type 'i32' and 'bool'" m2

  let (p3, m3) = errorInfo (T.UndefinedVariable pos "foo")
  assertEqual "type undefined pos" pos p3
  assertEqual "type undefined text" "undefined variable 'foo'" m3

  let (p4, m4) = errorInfo (T.UndefinedType pos "bar")
  assertEqual "type undefined type pos" pos p4
  assertEqual "type undefined type text" "undefined type 'bar'" m4

testTopLevelTypes :: IO ()
testTopLevelTypes = do
  assertLeft "top-level duplicate function items rejected" $ checkSource "f :: fn(x: i32) -> i32 { x }; f :: fn(x: i32) -> i32 { x };"
  assertLeft "duplicate declarations rejected" $ checkSource "x :: 1; x :: 2;"
  assertOk "direct recursion type checks" $ checkSource "fact :: fn(n: i32) -> i32 { if n == 0 { 1 } else { n * fact(n - 1) } };"
  assertOk "mutual recursion type checks" $ checkSource "even :: fn(n: i32) -> bool { if n == 0 { true } else { odd(n - 1) } }; odd :: fn(n: i32) -> bool { if n == 0 { false } else { even(n - 1) } };"
  assertOk "forward function reference type checks" $ checkSource "result: i32 = f(); f :: fn() -> i32 { 1 };"
  assertOk "function uses preceding global" $ checkSource "x :: 10; f :: fn() -> i32 { x }; result: i32 = f();"
  assertOk "function does capture local" $ checkSource "outer :: fn(x: i32) -> i32 { helper :: fn() -> i32 { x }; helper() };"
  assertLeft "if without else cannot produce int" $
    checkProgram
      ( Program
          [ TopLevelStmt
              ( ParsedStmt
                  pos0
                  ( ParsedDeclStmt
                      ( ParsedValueDecl
                          Mutable
                          (identFrom "x")
                          (Just intSyntax)
                          (expr (ParsedIfExpr (expr (ParsedBoolLit True)) (Block [] (Just (expr (ParsedIntLit 1)))) Nothing))
                      )
                  )
              )
          ]
      )

replUnitDecl :: ParsedRepl
replUnitDecl =
  Repl
    [ ReplStmt $
        ParsedStmt pos0 $
          ParsedDeclStmt $
            ParsedValueDecl
              Mutable
              (identFrom "x")
              Nothing
              (expr (ParsedIfExpr (expr (ParsedBoolLit False)) (Block [] Nothing) Nothing))
    ]

replUnitAddition :: ParsedRepl
replUnitAddition =
  Repl
    [ ReplExpr $
        expr $
          ParsedBinaryExpr AddOp (expr (ParsedVarExpr (identFrom "x"))) (expr (ParsedIntLit 5))
    ]

testReplTypes :: IO ()
testReplTypes = do
  assertEqual "repl expression type checks" (Right (Repl [ReplExpr (TypedExpr pos0 intType (TypedIntLit 1))])) $ checkRepl (Repl [ReplExpr (expr (ParsedIntLit 1))])
  assertLeft "repl remembers unit variable type" $ do
    (_, env) <- checkReplWithEnv [] replUnitDecl
    checkReplWithEnv env replUnitAddition
