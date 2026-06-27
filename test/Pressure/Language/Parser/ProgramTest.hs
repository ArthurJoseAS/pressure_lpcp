module Pressure.Language.Parser.ProgramTest (parserProgramTests) where

import Pressure.Language.Ast
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseProgram)
import Pressure.Language.Parser.TestUtil hiding (expect, parse, singleDecl)
import Pressure.Language.Types (Mutability (Constant, Mutable))
import Pressure.TestUtil (assertRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

parserProgramTests :: TestTree
parserProgramTests =
  testGroup
    "program"
    [ testCase "parses mutable declaration" testParseMutableDecl,
      testCase "parses constant declaration" testParseConstantDecl,
      testCase "parses function expression" testParseFnExpr
    ]

expect :: String -> Bool -> ParsedProgram -> IO ()
expect _ True _ = return ()
expect name False ast = error $ "unexpected AST for " ++ name ++ ": " ++ show ast

parse :: String -> String -> IO ParsedProgram
parse name source = assertRight name $ runAlex source parseProgram

singleDecl :: ParsedProgram -> Maybe ParsedDecl
singleDecl = \case
  Program [TopLevelStmt (ParsedStmt _ (ParsedDeclStmt decl))] -> Just decl
  _ -> Nothing

testParseMutableDecl :: IO ()
testParseMutableDecl = do
  ast <- parse "parse mutable declaration" "x: i32 = 42;"
  expect "mutable declaration" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedIntLit _))) -> isIntSyntax typ; _ -> False) ast

testParseConstantDecl :: IO ()
testParseConstantDecl = do
  ast <- parse "parse constant declaration" "x: i32 : 42;"
  expect "constant declaration" (case singleDecl ast of Just (ParsedValueDecl Constant _ (Just typ) (ParsedExpr _ (ParsedIntLit _))) -> isIntSyntax typ; _ -> False) ast

testParseFnExpr :: IO ()
testParseFnExpr = do
  ast <- parse "parse function expression" "add :: fn(a: i32, b: i32) -> i32 { a + b };"
  expect "function expression" (case singleDecl ast of Just (ParsedValueDecl Constant _ Nothing (ParsedExpr _ (ParsedFnExpr [Param _ p1, Param _ p2] ret _))) -> isIntSyntax p1 && isIntSyntax p2 && isIntSyntax ret; _ -> False) ast
