module Pressure.BuiltinsTest (builtinsTests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Pressure.Interpreter.Error qualified as Eval
import Pressure.Interpreter.Eval qualified as Eval
import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseRepl)
import Pressure.Language.Types
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
      testCase "builtin names are available in REPL" testBuiltinsAvailable,
      testCase "@as casts i32 to i64" testAsIntToInt,
      testCase "@as casts i32 to f64" testAsIntToFloat,
      testCase "@as casts f64 to i32" testAsFloatToInt,
      testCase "@as casts bool to i32" testAsBoolToInt,
      testCase "@as casts i32 to bool" testAsIntToBool,
      testCase "@as casts string to string" testAsStringToString,
      testCase "@as with type argument" testAsTypeToType,
      testCase "@as rejects cast to function type" testAsRejectFnCast,
      testCase "@as rejects non-type first argument" testAsRejectNonTypeArg,
      testCase "@as rejects wrong arity" testAsRejectWrongArity,
      testCase "@as casts string to int" testAsStringToInt,
      testCase "@as casts string to float" testAsStringToFloat,
      testCase "@as casts int to string" testAsIntToString,
      testCase "@as casts bool to string" testAsBoolToString,
      testCase "@as rejects invalid string to int" testAsInvalidStringToInt,
      testCase "@as rejects cast to anytype" testAsRejectAnyType,
      testCase "anytype annotation resolves to inferred type" testAnyTypeAnnotation
    ]

runStdlibEval :: String -> IO (Either Eval.EvalError Value)
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
      | msg == "argument 1 has non-printable type 'fn(string) -> unit'" -> return ()
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
    Right _ -> error "expected type error but succeeded"

testReadType :: IO ()
testReadType = do
  checkOk "@read type" "x: string = @read();"

testBuiltinsAvailable :: IO ()
testBuiltinsAvailable = do
  checkOk "builtin read available" "@read();"
  checkOk "builtin printf available" "@printf(\"\");"
  checkOk "builtin as available" "@as(i32, 42);"

testAsIntToInt :: IO ()
testAsIntToInt = do
  checkOk "@as int to int" "x: i64 = @as(i64, 42);"
  withTokens "@as int to int eval" "x: i64 = @as(i64, 42);" $ \ast -> do
    result <- evalParsed "@as int to int eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I64 42) -> return ()
          other -> error $ "expected x = 42 (i64), got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsIntToFloat :: IO ()
testAsIntToFloat = do
  checkOk "@as int to float" "x: f64 = @as(f64, 42);"
  withTokens "@as int to float eval" "x: f64 = @as(f64, 42);" $ \ast -> do
    result <- evalParsed "@as int to float eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VFloat F64 42.0) -> return ()
          other -> error $ "expected x = 42.0 (f64), got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsFloatToInt :: IO ()
testAsFloatToInt = do
  checkOk "@as float to int" "x: i32 = @as(i32, 3.14);"
  withTokens "@as float to int eval" "x: i32 = @as(i32, 3.14);" $ \ast -> do
    result <- evalParsed "@as float to int eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected x = 3 (i32), got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsBoolToInt :: IO ()
testAsBoolToInt = do
  checkOk "@as bool to int" "x: i32 = @as(i32, true);"
  withTokens "@as bool to int eval" "x: i32 = @as(i32, true);" $ \ast -> do
    result <- evalParsed "@as bool to int eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1 (i32), got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsIntToBool :: IO ()
testAsIntToBool = do
  checkOk "@as int to bool" "x: bool = @as(bool, 0);"
  withTokens "@as int to bool eval" "x: bool = @as(bool, 0);" $ \ast -> do
    result <- evalParsed "@as int to bool eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool False) -> return ()
          other -> error $ "expected x = false, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsStringToString :: IO ()
testAsStringToString = do
  checkOk "@as string to string" "x: string = @as(string, \"hello\");"
  withTokens "@as string to string eval" "x: string = @as(string, \"hello\");" $ \ast -> do
    result <- evalParsed "@as string to string eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VString "hello") -> return ()
          other -> error $ "expected x = \"hello\", got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsTypeToType :: IO ()
testAsTypeToType = do
  checkOk "@as type to type" "x: type : @as(type, i32);"

testAsRejectFnCast :: IO ()
testAsRejectFnCast = do
  assertTypeError "@as(fn() -> unit, 42);" (Type.InvalidCast pos0 (FnT [] UnitT) (IntT Signed I32))

testAsRejectNonTypeArg :: IO ()
testAsRejectNonTypeArg = do
  assertTypeError "@as(42, 1);" (Type.InvalidCast pos0 (IntT Signed I32) (IntT Signed I32))

testAsRejectWrongArity :: IO ()
testAsRejectWrongArity = do
  assertTypeError "@as(i32);" (Type.InvalidCast pos0 TypeT UnitT)
  assertTypeError "@as(i32, 1, 2);" (Type.InvalidCast pos0 TypeT UnitT)

testAsStringToInt :: IO ()
testAsStringToInt = do
  checkOk "@as string to int" "x: i32 = @as(i32, \"42\");"
  withTokens "@as string to int eval" "x: i32 = @as(i32, \"42\");" $ \ast -> do
    result <- evalParsed "@as string to int eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsStringToFloat :: IO ()
testAsStringToFloat = do
  checkOk "@as string to float" "x: f64 = @as(f64, \"3.14\");"
  withTokens "@as string to float eval" "x: f64 = @as(f64, \"3.14\");" $ \ast -> do
    result <- evalParsed "@as string to float eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VFloat F64 3.14) -> return ()
          other -> error $ "expected x = 3.14, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsIntToString :: IO ()
testAsIntToString = do
  checkOk "@as int to string" "x: string = @as(string, 42);"
  withTokens "@as int to string eval" "x: string = @as(string, 42);" $ \ast -> do
    result <- evalParsed "@as int to string eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VString "42") -> return ()
          other -> error $ "expected x = \"42\", got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsBoolToString :: IO ()
testAsBoolToString = do
  checkOk "@as bool to string" "x: string = @as(string, true);"
  withTokens "@as bool to string eval" "x: string = @as(string, true);" $ \ast -> do
    result <- evalParsed "@as bool to string eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VString "true") -> return ()
          other -> error $ "expected x = \"true\", got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAsInvalidStringToInt :: IO ()
testAsInvalidStringToInt = do
  ast <- case runAlex "x: i32 = @as(i32, \"notanumber\");" parseRepl of
    Left err -> error $ "parse failed: " ++ err
    Right ast -> return ast
  case TC.checkRepl ast of
    Right typedAst -> do
      result <- runExceptT (runStateT (Eval.evalRepl typedAst) emptyEnv)
      case result of
        Left (Eval.RuntimeError (Eval.CastError _ _)) -> return ()
        Left other -> error $ "expected CastError, got " ++ show other
        Right _ -> error $ "expected runtime error but succeeded"
    Left err -> error $ "type check failed: " ++ show err

testAsRejectAnyType :: IO ()
testAsRejectAnyType = do
  assertTypeError "@as(anytype, 42);" (Type.InvalidCast pos0 AnyTypeT (IntT Signed I32))

testAnyTypeAnnotation :: IO ()
testAnyTypeAnnotation = do
  checkOk "anytype annotation resolves" "x: anytype = 42;"
  withTokens "anytype annotation eval" "x: anytype = 42;" $ \ast -> do
    result <- evalParsed "anytype annotation" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err
