module Pressure.Typechecker.ArithTest (arithTypeTests) where

import Pressure.Language.Ast
import Pressure.Language.Types
import Pressure.TestUtil (assertEqual, assertLeft, checkOk, pos0)
import Pressure.Typechecker.Check (checkExpr)
import Pressure.Typechecker.Error (Error)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

arithTypeTests :: TestTree
arithTypeTests =
  testGroup
    "arithmetic"
    [ testCase "checks int addition" testIntAdd,
      testCase "checks float addition" testFloatAdd,
      testCase "checks int division" testIntDiv,
      testCase "checks float division" testFloatDiv,
      testCase "checks type-name annotations" testTypeNameAnnotation,
      testCase "checks int multiplication" testIntMul,
      testCase "checks float multiplication" testFloatMul,
      testCase "checks int subtraction" testIntSub,
      testCase "checks float subtraction" testFloatSub,
      testCase "checks binary expression types" testBinaryExprTypes,
      testCase "checks unary expression types" testUnaryExprTypes
    ]

intType :: Type
intType = IntT Signed I32

expr :: ParsedExprKind -> ParsedExpr
expr = ParsedExpr pos0

checkExprType :: ParsedExpr -> Either Error Type
checkExprType = fmap typedExprType . checkExpr

testIntAdd :: IO ()
testIntAdd = checkOk "int addition" "x: int = 1 + 2;"

testFloatAdd :: IO ()
testFloatAdd = checkOk "float addition" "x: float = 1.0 + 2.0;"

testIntDiv :: IO ()
testIntDiv = checkOk "int division" "x: int = 8 / 4;"

testFloatDiv :: IO ()
testFloatDiv = checkOk "float division" "x: float = 3.0 / 2.0;"

testTypeNameAnnotation :: IO ()
testTypeNameAnnotation = checkOk "TypeName annotation" "x: i32 = 42;"

testIntMul :: IO ()
testIntMul = checkOk "int multiplication" "x: int = 3 * 4;"

testFloatMul :: IO ()
testFloatMul = checkOk "float multiplication" "x: float = 1.5 * 2.0;"

testIntSub :: IO ()
testIntSub = checkOk "int subtraction" "x: int = 8 - 3;"

testFloatSub :: IO ()
testFloatSub = checkOk "float subtraction" "x: float = 8.5 - 3.0;"

testBinaryExprTypes :: IO ()
testBinaryExprTypes = do
  assertEqual "int addition type" (Right intType) $ checkExprType (expr (ParsedBinaryExpr AddOp (expr (ParsedIntLit 1)) (expr (ParsedIntLit 2))))
  assertLeft "bool arithmetic unsupported" $ checkExprType (expr (ParsedBinaryExpr AddOp (expr (ParsedBoolLit True)) (expr (ParsedIntLit 1))))
  assertEqual "boolean and type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr AndOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))
  assertEqual "comparison type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr LtOp (expr (ParsedIntLit 1)) (expr (ParsedIntLit 2))))
  assertEqual "equality type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr EqOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))
  assertLeft "ordered bool comparison unsupported" $ checkExprType (expr (ParsedBinaryExpr LtOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))

testUnaryExprTypes :: IO ()
testUnaryExprTypes = do
  assertEqual "unary negation type" (Right intType) $ checkExprType (expr (ParsedUnaryExpr NegOp (expr (ParsedIntLit 1))))
  assertEqual "unary not type" (Right BoolT) $ checkExprType (expr (ParsedUnaryExpr NotOp (expr (ParsedBoolLit False))))
  assertLeft "unary negation bool unsupported" $ checkExprType (expr (ParsedUnaryExpr NegOp (expr (ParsedBoolLit True))))
  assertLeft "unary not int unsupported" $ checkExprType (expr (ParsedUnaryExpr NotOp (expr (ParsedIntLit 1))))
  assertLeft "unary ampersand unsupported" $ checkExprType (expr (ParsedUnaryExpr AmpersandOp (expr (ParsedIntLit 1))))
