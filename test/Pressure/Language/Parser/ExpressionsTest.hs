module Pressure.Language.Parser.ExpressionsTest (parserExpressionTests) where

import Pressure.Language.Ast
import Pressure.Language.Parser.TestUtil
import Pressure.Language.Types (BinaryOp (..), Mutability (Mutable), UnaryOp (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

parserExpressionTests :: TestTree
parserExpressionTests =
  testGroup
    "expressions"
    [ testCase "parses addition expression" testParseAdditionExpr,
      testCase "parses multiplication precedence" testParseMulPrecedence,
      testCase "parses division expression" testParseDivisionExpr,
      testCase "parses parenthesized expression" testParseParenExpr,
      testCase "parses bare expression" testParseBareExpr,
      testCase "parses variable reference" testParseVarRef,
      testCase "parses subtraction precedence" testParseSubPrecedence,
      testCase "parses boolean precedence" testParseBoolPrecedence,
      testCase "parses comparison after arithmetic" testParseComparisonPrecedence,
      testCase "parses unary negation" testParseUnaryNeg,
      testCase "parses unary not" testParseUnaryNot,
      testCase "parses unary ampersand" testParseUnaryAmpersand,
      testCase "parses unary precedence" testParseUnaryPrecedence,
      testCase "parses call expression" testParseCallExpr
    ]

testParseAdditionExpr :: IO ()
testParseAdditionExpr = do
  ast <- parse "parse addition expression" "sum: i32 = 1 + 2 + 3;"
  expect "addition expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3))))) -> isIntSyntax typ; _ -> False) ast

testParseMulPrecedence :: IO ()
testParseMulPrecedence = do
  ast <- parse "parse multiplication precedence" "value: i32 = 1 + 2 * 3;"
  expect "multiplication precedence" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3))))))) -> isIntSyntax typ; _ -> False) ast

testParseDivisionExpr :: IO ()
testParseDivisionExpr = do
  ast <- parse "parse division expression" "value: i32 = 8 / 4 / 2;"
  expect "division expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedIntLit 8)) (ParsedExpr _ (ParsedIntLit 4)))) (ParsedExpr _ (ParsedIntLit 2))))) -> isIntSyntax typ; _ -> False) ast

testParseParenExpr :: IO ()
testParseParenExpr = do
  ast <- parse "parse parenthesized expression" "value: i32 = (1 + 2) * 3;"
  expect "parenthesized expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3))))) -> isIntSyntax typ; _ -> False) ast

testParseBareExpr :: IO ()
testParseBareExpr = do
  ast <- parse "parse bare expression" "1 + 2;"
  expect "bare expression" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) -> True; _ -> False) ast

testParseVarRef :: IO ()
testParseVarRef = do
  ast <- parse "parse variable reference as expression" "x;"
  expect "variable reference" (case singleExpr ast of Just (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) -> True; _ -> False) ast

testParseSubPrecedence :: IO ()
testParseSubPrecedence = do
  ast <- parse "parse subtraction precedence" "value: i32 = 1 - 2 * 3;"
  expect "subtraction precedence" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr SubOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3))))))) -> isIntSyntax typ; _ -> False) ast

testParseBoolPrecedence :: IO ()
testParseBoolPrecedence = do
  ast <- parse "parse boolean precedence" "true or false and 1 < 2;"
  expect "boolean precedence" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr OrOp _ _)) -> True; _ -> False) ast

testParseComparisonPrecedence :: IO ()
testParseComparisonPrecedence = do
  ast <- parse "parse comparison after arithmetic" "1 + 2 * 3 == 7;"
  expect "comparison precedence" (case singleExpr ast of Just e -> isBinary EqOp (isBinary AddOp (isIntLit 1) (isBinary MulOp (isIntLit 2) (isIntLit 3))) (isIntLit 7) e; _ -> False) ast

testParseUnaryNeg :: IO ()
testParseUnaryNeg = do
  ast <- parse "parse unary negation" "-1;"
  expect "unary negation" (case singleExpr ast of Just (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 1)))) -> True; _ -> False) ast

testParseUnaryNot :: IO ()
testParseUnaryNot = do
  ast <- parse "parse unary not" "!false;"
  expect "unary not" (case singleExpr ast of Just (ParsedExpr _ (ParsedUnaryExpr NotOp (ParsedExpr _ (ParsedBoolLit False)))) -> True; _ -> False) ast

testParseUnaryAmpersand :: IO ()
testParseUnaryAmpersand = do
  ast <- parse "parse unary ampersand" "&x;"
  expect "unary ampersand" (case singleExpr ast of Just (ParsedExpr _ (ParsedUnaryExpr AmpersandOp (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))))) -> True; _ -> False) ast

testParseUnaryPrecedence :: IO ()
testParseUnaryPrecedence = do
  ast <- parse "parse unary precedence" "1 * -2;"
  expect "unary precedence" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 2)))))) -> True; _ -> False) ast

testParseCallExpr :: IO ()
testParseCallExpr = do
  ast <- parse "parse call expression" "add(1, 2);"
  expect "call expression" (case singleExpr ast of Just (ParsedExpr _ (ParsedCallExpr (ParsedExpr _ (ParsedVarExpr (Ident _ "add"))) [ParsedExpr _ (ParsedIntLit 1), ParsedExpr _ (ParsedIntLit 2)])) -> True; _ -> False) ast
