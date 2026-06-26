module Pressure.Language.Parser.ReplTest (parserReplTests) where

import Pressure.Language.Ast
import Pressure.Language.Parser.TestUtil
import Pressure.Language.Types (BinaryOp (AddOp), Mutability (Mutable))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

parserReplTests :: TestTree
parserReplTests =
  testGroup
    "repl"
    [ testCase "parses declaration without semicolon" testParseDeclNoSemicolon,
      testCase "parses declaration with semicolon" testParseDeclSemicolon,
      testCase "parses bare expression" testParseBareReplExpr,
      testCase "parses expression with semicolon" testParseReplExprSemicolon,
      testCase "parses variable reference" testParseVarRef,
      testCase "parses variable in expression" testParseVarInExpr
    ]

testParseDeclNoSemicolon :: IO ()
testParseDeclNoSemicolon = do
  ast <- parse "repl: declaration without semicolon" "x: int = 42"
  expect "repl declaration" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedIntLit 42))) -> isIntSyntax typ; _ -> False) ast

testParseDeclSemicolon :: IO ()
testParseDeclSemicolon = do
  ast <- parse "repl: declaration with semicolon" "x: int = 42;"
  expect "repl declaration stmt" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedIntLit 42))) -> isIntSyntax typ; _ -> False) ast

testParseBareReplExpr :: IO ()
testParseBareReplExpr = do
  ast <- parse "repl: bare expression" "1 + 2"
  expect "repl expression" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) -> True; _ -> False) ast

testParseReplExprSemicolon :: IO ()
testParseReplExprSemicolon = do
  ast <- parse "repl: expression with semicolon" "1 + 2;"
  expect "repl expression stmt" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) -> True; _ -> False) ast

testParseVarRef :: IO ()
testParseVarRef = do
  ast <- parse "repl: variable reference" "x"
  expect "repl variable reference" (case singleExpr ast of Just (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) -> True; _ -> False) ast

testParseVarInExpr :: IO ()
testParseVarInExpr = do
  ast <- parse "repl: variable in expression" "x + 1"
  expect "repl variable in expr" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) (ParsedExpr _ (ParsedIntLit 1)))) -> True; _ -> False) ast
