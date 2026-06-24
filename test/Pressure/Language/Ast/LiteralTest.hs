module Pressure.Language.Ast.LiteralTest
  ( literalTests,
  )
where

import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Ast
import Pressure.Language.Types
import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

literalTests :: TestTree
literalTests =
  testGroup
    "literals"
    [ testCase "evaluates int literals" testIntLit,
      testCase "evaluates float literals" testFloatLit,
      testCase "evaluates bool literals" testBoolLit,
      testCase "evaluates string literals" testStringLit,
      testCase "declares and looks up variables" testVarDeclAndLookup
    ]

testIntLit :: IO ()
testIntLit = do
  checkOk "int literal" "x: int = 42;"
  withTokens "int literal eval" "x: int = 42;" $ \ast -> do
    result <- evalParsed "int literal eval" ast
    case result of
      Left err -> error $ "int literal eval failed: " ++ show err
      Right (val, env) -> do
        if val == VUnit then return () else error $ "expected VUnit got " ++ show val
        case lookupValue "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other

testFloatLit :: IO ()
testFloatLit = do
  checkOk "float literal" "x: float = 3.14;"
  withTokens "float literal eval" "x: float = 3.14;" $ \ast -> do
    result <- evalParsed "float literal eval" ast
    case result of
      Left err -> error $ "float literal eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VFloat F64 3.14) -> return ()
          other -> error $ "expected 3.14, got " ++ show other

testBoolLit :: IO ()
testBoolLit = do
  checkOk "bool literal" "x: bool = true;"
  withTokens "bool literal eval" "x: bool = true;" $ \ast -> do
    result <- evalParsed "bool literal eval" ast
    case result of
      Left err -> error $ "bool literal eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected true, got " ++ show other

testVarDeclAndLookup :: IO ()
testVarDeclAndLookup = do
  let decl42 = "x: int = 42;"
  withTokens "parse decl42" decl42 $ \ast -> do
    result <- evalParsed "var decl and lookup" ast
    case result of
      Right (_, env) ->
        assertExpr "x after decl" (TypedExpr pos0 UnitT (TypedVarExpr (identFrom "x"))) env (VInt Signed I32 42)
      Left err -> error $ "eval failed: " ++ show err

testStringLit :: IO ()
testStringLit = do
  checkOk "string literal" "x: string = \"hello\";"
  withTokens "string literal eval" "x: string = \"hello\";" $ \ast -> do
    result <- evalParsed "string literal eval" ast
    case result of
      Left err -> error $ "string literal eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VString "hello") -> return ()
          other -> error $ "expected \"hello\", got " ++ show other
