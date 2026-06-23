module Pressure.BuiltinsTest (builtinsTests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Pressure.Interpreter.Eval qualified as Eval
import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseRepl)
import Pressure.TestUtil
import Pressure.Typechecker qualified as TC
import Pressure.Typechecker.Error qualified as Type
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

builtinsTests :: TestTree
builtinsTests =
  testGroup
    "builtins"
    [ testCase "rejects @printf with non-string first arg" testPrintfNonStringFirstArg,
      testCase "rejects @printf with wrong placeholder count" testPrintfWrongPlaceholderCount,
      testCase "rejects @printf with non-printable arg" testPrintfNonPrintableArg,
      testCase "accepts @printf with correct usage" testPrintfCorrect,
      testCase "accepts @printf with no placeholders" testPrintfNoPlaceholders,
      testCase "rejects @printf with no arguments" testPrintfNoArgs,
      testCase "rejects @printf with non-literal format" testPrintfNonLiteralFormat,
      testCase "accepts @read type" testReadType,
      testCase "builtin names are available in REPL" testBuiltinsAvailable
    ]

runStdlibEval :: String -> IO (Either Eval.Error Value)
runStdlibEval source = do
  ast <- case runAlex source parseRepl of
    Left err -> error $ "parse failed: " ++ err
    Right ast -> return ast
  case TC.checkRepl ast of
    Left err -> error $ "type check failed: " ++ show err
    Right typedAst -> do
      result <- runExceptT (runStateT (Eval.evalRepl typedAst) emptyEnv)
      case result of
        Left err -> return $ Left err
        Right (val, _) -> return $ Right val

assertTypeError :: String -> Type.Error -> IO ()
assertTypeError source expectedErr = do
  ast <- case runAlex source parseRepl of
    Left err -> error $ "parse failed: " ++ err
    Right ast -> return ast
  case TC.checkRepl ast of
    Left err -> assertEqual "type error" expectedErr err
    Right _ -> error $ "expected type error " ++ show expectedErr ++ " but succeeded"

testPrintfNonStringFirstArg :: IO ()
testPrintfNonStringFirstArg =
  assertTypeError "@printf(42);" (Type.InvalidPrintf pos0 "first argument must be a string")

testPrintfWrongPlaceholderCount :: IO ()
testPrintfWrongPlaceholderCount =
  assertTypeError "@printf(\"{} {}\", 1);" (Type.InvalidPrintf pos0 "expected 2 arguments for placeholders, got 1")

testPrintfNonPrintableArg :: IO ()
testPrintfNonPrintableArg = do
  ast <- case runAlex "@printf(\"{}\", @printf);" parseRepl of
    Left err -> error $ "parse failed: " ++ err
    Right ast -> return ast
  case TC.checkRepl ast of
    Left (Type.InvalidPrintf _ msg)
      | msg == "argument 1 has non-printable type 'fn(string) -> ()'" -> return ()
    other -> error $ "expected InvalidPrintf about non-printable, got " ++ show other

testPrintfCorrect :: IO ()
testPrintfCorrect = do
  result <- runStdlibEval "@printf(\"hello\");"
  case result of
    Right VUnit -> return ()
    Right v -> error $ "expected VUnit, got " ++ show v
    Left err -> error $ "eval failed: " ++ show err

testPrintfNoPlaceholders :: IO ()
testPrintfNoPlaceholders = do
  result <- runStdlibEval "@printf(\"hello\");"
  case result of
    Right VUnit -> return ()
    Right v -> error $ "expected VUnit, got " ++ show v
    Left err -> error $ "eval failed: " ++ show err

testPrintfNoArgs :: IO ()
testPrintfNoArgs =
  assertTypeError "@printf();" (Type.InvalidPrintf pos0 "expected at least a format string argument")

testPrintfNonLiteralFormat :: IO ()
testPrintfNonLiteralFormat = do
  ast <- case runAlex "fmt: string = \"hello\"; @printf(fmt);" parseRepl of
    Left err -> error $ "parse failed: " ++ err
    Right ast -> return ast
  case TC.checkRepl ast of
    Left (Type.InvalidPrintf _ "format string must be a literal") -> return ()
    Left other -> error $ "expected InvalidPrintf about literal, got " ++ show other
    Right _ -> error $ "expected type error but succeeded"

testReadType :: IO ()
testReadType = do
  checkOk "@read type" "x: string = @read();"

testBuiltinsAvailable :: IO ()
testBuiltinsAvailable = do
  checkOk "builtin read available" "@read();"
  checkOk "builtin printf available" "@printf(\"\");"
