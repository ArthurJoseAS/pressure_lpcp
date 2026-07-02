module Pressure.Interpreter.PointerTest
  ( pointerTests,
  )
where

import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Types
import Pressure.TestUtil
import Pressure.Typechecker (checkRepl)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

pointerTests :: TestTree
pointerTests =
  testGroup
    "pointers"
    [ testCase "evaluates &x as a value" testEvalAddrOf,
      testCase "dereferences a pointer" testEvalDeref,
      testCase "writing through *mut updates the original" testWriteThroughMutPtr,
      testCase "writing through *T is not exposed at runtime" testWriteThroughImmutablePtr,
      testCase "address of a struct field points to the field" testAddrOfStructField,
      testCase "address of an array round-trips on reassignment" testAddrOfArray,
      testCase "function parameter pointer mutates the caller's binding" testParamPointer,
      testCase "function parameter pointer reads from the caller's binding" testParamPointerRead,
      testCase "function parameter struct-field pointer mutates the caller" testParamStructPointer
    ]

testEvalAddrOf :: IO ()
testEvalAddrOf =
  withTokens "eval &x" "x : int = 24; p : *int = &x;" $ \ast -> do
    result <- evalParsed "eval &x" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "p" env of
          Just (VPtr Constant _ []) -> return ()
          other -> error $ "expected immutable pointer to x, got " ++ show other

testEvalDeref :: IO ()
testEvalDeref =
  withTokens "eval p.*" "x : int = 24; p : *int = &x; v : int = p.*;" $ \ast -> do
    result <- evalParsed "eval p.*" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "v" env of
          Just (VInt Signed I32 24) -> return ()
          other -> error $ "expected v = 24, got " ++ show other

testWriteThroughMutPtr :: IO ()
testWriteThroughMutPtr =
  withTokens "write through *mut" "x : int = 24; p : *mut int = &mut x; p.* = 42;" $ \ast -> do
    result <- evalParsed "write through *mut" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other

testWriteThroughImmutablePtr :: IO ()
testWriteThroughImmutablePtr =
  withTokens "write through *T rejected" "x : int = 24; p : *int = &x; p.* = 42;" $ \ast -> do
    case checkRepl ast of
      Left _ -> return ()
      Right _ -> error "expected type error for write through *T"

testAddrOfStructField :: IO ()
testAddrOfStructField =
  withTokens
    "&s.x round-trips"
    "S :: struct { v : int }; s : S = .{ v = 1 }; p : *mut int = &mut s.v; p.* = 5;"
    $ \ast -> do
      result <- evalParsed "&s.x round-trips" ast
      case result of
        Left err -> error $ "eval failed: " ++ show err
        Right (_, env) ->
          case lookupValue "s" env of
            Just (VStruct [("v", VInt Signed I32 5)]) -> return ()
            other -> error $ "expected s.v = 5, got " ++ show other

testAddrOfArray :: IO ()
testAddrOfArray =
  withTokens
    "&arr round-trips on reassignment"
    "arr : []i32 = [1, 2, 3]; p : *mut []i32 = &mut arr; p.* = [4, 5, 6];"
    $ \ast -> do
      result <- evalParsed "&arr round-trips on reassignment" ast
      case result of
        Left err -> error $ "eval failed: " ++ show err
        Right (_, env) ->
          case lookupValue "arr" env of
            Just (VArray [VInt Signed I32 4, VInt Signed I32 5, VInt Signed I32 6]) -> return ()
            other -> error $ "expected arr = [4,5,6], got " ++ show other

testParamPointer :: IO ()
testParamPointer =
  withTokens
    "function param mutates caller"
    "inc :: fn(p: *mut int) { p.* = p.* + 1; }; main :: fn() { x : int = 10; inc(&mut x); };"
    $ \_ -> do
      result <-
        evalProgramFromSource
          "inc :: fn(p: *mut int) { p.* = p.* + 1; }; main :: fn() { x : int = 10; inc(&mut x); @printf(\"{}\", x); };"
      case result of
        Right _ -> return ()
        Left err -> error $ "expected success, got " ++ show err

testParamPointerRead :: IO ()
testParamPointerRead =
  withTokens
    "function param reads caller"
    "show_it :: fn(p: *int) { @printf(\"{}\", p.*); }; main :: fn() { x : int = 10; show_it(&x); };"
    $ \_ -> do
      result <-
        evalProgramFromSource
          "show_it :: fn(p: *int) { @printf(\"{}\", p.*); }; main :: fn() { x : int = 10; show_it(&x); };"
      case result of
        Right _ -> return ()
        Left err -> error $ "expected success, got " ++ show err

testParamStructPointer :: IO ()
testParamStructPointer =
  withTokens
    "function param struct-field pointer"
    "reset :: fn(p: *mut int) { p.* = 0; }; main :: fn() { S :: struct { v : int }; s : S = .{ v = 42 }; reset(&mut s.v); };"
    $ \_ -> do
      result <-
        evalProgramFromSource
          "reset :: fn(p: *mut int) { p.* = 0; }; main :: fn() { S :: struct { v : int }; s : S = .{ v = 42 }; reset(&mut s.v); @printf(\"{}\", s.v); };"
      case result of
        Right _ -> return ()
        Left err -> error $ "expected success, got " ++ show err
