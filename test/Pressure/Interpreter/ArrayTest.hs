module Pressure.Interpreter.ArrayTest
  ( arrayTests,
  )
where

import Control.Exception (ErrorCall, SomeException, fromException, throwIO, toException, try)
import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseRepl)
import Pressure.Language.Types
import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

arrayTests :: TestTree
arrayTests =
  testGroup
    "arrays"
    [ testCase "evaluates int array literal" testIntArrayLit
    , testCase "evaluates empty array literal" testEmptyArrayLit
    , testCase "evaluates nested array literal" testNestedArrayLit
    , testCase "evaluates bool array literal" testBoolArrayLit
    , testCase "evaluates string array literal" testStringArrayLit
    , testCase "indexes array first element" testIndexFirst
    , testCase "indexes array last element" testIndexLast
    , testCase "indexes nested array" testIndexNested
    , testCase "indexes with variable index" testIndexWithVar
    , testCase "Show VArray renders elements joined by commas" testShowVArray
    , testCase "panics on negative index" testPanicNegativeIndex
    , testCase "panics on out-of-bounds index" testPanicOobIndex
    , testCase "panics on indexing non-array" testPanicIndexNonArray
    , testCase "panics on indexing with non-int" testPanicIndexNonInt
    ]

testIntArrayLit :: IO ()
testIntArrayLit =
  withTokens "int array literal eval" "x: []i32 = [1, 2, 3];" $ \ast -> do
    result <- evalParsed "int array literal eval" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VArray [VInt Signed I32 1, VInt Signed I32 2, VInt Signed I32 3]) -> return ()
          other -> error $ "expected VArray [1,2,3], got " ++ show other

testEmptyArrayLit :: IO ()
testEmptyArrayLit =
  withTokens "empty array literal eval" "x: []i32 = [];" $ \ast -> do
    result <- evalParsed "empty array literal eval" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VArray []) -> return ()
          other -> error $ "expected VArray [], got " ++ show other

testNestedArrayLit :: IO ()
testNestedArrayLit =
  withTokens "nested array literal eval" "x: [][]i32 = [[1, 2], [3]];" $ \ast -> do
    result <- evalParsed "nested array literal eval" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just
            ( VArray
                [ VArray [VInt Signed I32 1, VInt Signed I32 2],
                  VArray [VInt Signed I32 3]
                ]
            ) ->
              return ()
          other -> error $ "expected nested VArray, got " ++ show other

testBoolArrayLit :: IO ()
testBoolArrayLit =
  withTokens "bool array literal eval" "x: []bool = [true, false, true];" $ \ast -> do
    result <- evalParsed "bool array literal eval" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VArray [VBool True, VBool False, VBool True]) -> return ()
          other -> error $ "expected VArray [true, false, true], got " ++ show other

testStringArrayLit :: IO ()
testStringArrayLit =
  withTokens "string array literal eval" "x: []string = [\"a\", \"b\"];" $ \ast -> do
    result <- evalParsed "string array literal eval" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VArray [VString "a", VString "b"]) -> return ()
          other -> error $ "expected VArray [a, b], got " ++ show other

testIndexFirst :: IO ()
testIndexFirst =
  withTokens "index first" "x: []i32 = [10, 20, 30]; x[0];" $ \ast -> do
    result <- evalParsed "index first" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (val, _) ->
        assertEqual "x[0]" (VInt Signed I32 10) val

testIndexLast :: IO ()
testIndexLast =
  withTokens "index last" "x: []i32 = [10, 20, 30]; x[2];" $ \ast -> do
    result <- evalParsed "index last" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (val, _) ->
        assertEqual "x[2]" (VInt Signed I32 30) val

testIndexNested :: IO ()
testIndexNested =
  withTokens "index nested" "mat: [][]i32 = [[1, 2], [3, 4]]; mat[1][0];" $ \ast -> do
    result <- evalParsed "index nested" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (val, _) ->
        assertEqual "mat[1][0]" (VInt Signed I32 3) val

testIndexWithVar :: IO ()
testIndexWithVar =
  withTokens "index with var" "x: []i32 = [10, 20, 30]; i: i32 = 2; x[i];" $ \ast -> do
    result <- evalParsed "index with var" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (val, _) ->
        assertEqual "x[i]" (VInt Signed I32 30) val

testShowVArray :: IO ()
testShowVArray = do
  show (VArray [VInt Signed I32 1, VInt Signed I32 2, VInt Signed I32 3])
    @?= "[1,2,3]"
  show (VArray []) @?= "[]"
  show (VArray [VBool True, VBool False]) @?= "[true,false]"

assertPanicOnEval :: String -> String -> IO ()
assertPanicOnEval name source = do
  ast <- case runAlex source parseRepl of
    Left err -> error $ name ++ ": parse failed: " ++ err
    Right ast -> return ast
  result <- try @SomeException (evalParsed name ast)
  case result of
    Right _ ->
      error $ name ++ ": expected panic during eval but eval succeeded"
    Left e ->
      case fromException (toException e) :: Maybe ErrorCall of
        Just _ -> return ()
        Nothing -> throwIO e

testPanicNegativeIndex :: IO ()
testPanicNegativeIndex =
  assertPanicOnEval
    "negative index"
    "x: []i32 = [1, 2, 3]; x[-1];"

testPanicOobIndex :: IO ()
testPanicOobIndex =
  assertPanicOnEval
    "out-of-bounds index"
    "x: []i32 = [1, 2, 3]; x[3];"

testPanicIndexNonArray :: IO ()
testPanicIndexNonArray =
  assertPanicOnEval
    "indexing non-array"
    "x: i32 = 5; x[0];"

testPanicIndexNonInt :: IO ()
testPanicIndexNonInt =
  assertPanicOnEval
    "indexing with non-int"
    "x: []i32 = [1, 2, 3]; x[true];"
