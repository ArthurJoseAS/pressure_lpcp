module Pressure.Typechecker.FunctionTest (functionTypeTests) where

import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

functionTypeTests :: TestTree
functionTypeTests =
  testGroup
    "functions"
    [ testCase "rejects same-block closure capture" testClosureCapturesByValue,
      testCase "accepts local mutual recursion" testLocalMutualRecursion,
      testCase "accepts nested function capture" testNestedFunctionCapture
    ]

testClosureCapturesByValue :: IO ()
testClosureCapturesByValue = checkErr "closure does not capture same-block variable" "x :: 10; addX :: fn(y: i32) -> i32 { x + y }; x :: 20; result: i32 = addX(5);"

testLocalMutualRecursion :: IO ()
testLocalMutualRecursion = checkOk "local mutual recursion" "outer :: fn(n: i32) -> bool { even :: fn(x: i32) -> bool { if x == 0 { true } else { odd(x - 1) } }; odd :: fn(x: i32) -> bool { if x == 0 { false } else { even(x - 1) } }; even(n) }; result: bool = outer(9);"

testNestedFunctionCapture :: IO ()
testNestedFunctionCapture = checkOk "nested function capture" "outer :: fn(x: i32) -> i32 { helper :: fn(n: i32) -> i32 { if n == 0 { x } else { helper(n - 1) } }; helper(3) }; result: i32 = outer(7);"
