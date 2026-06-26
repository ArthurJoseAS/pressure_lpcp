module Pressure.Typechecker.AssignTest (assignTypeTests) where

import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

assignTypeTests :: TestTree
assignTypeTests =
  testGroup
    "assignment"
    [ testCase "rejects assignment to constants" testAssignToConstant,
      testCase "rejects assignment to undefined" testAssignToUndefined,
      testCase "rejects assignment type mismatch" testAssignTypeMismatch
    ]

testAssignToConstant :: IO ()
testAssignToConstant = checkErr "assign to constant" "x :: 42; x = 10;"

testAssignToUndefined :: IO ()
testAssignToUndefined = checkErr "assign to undefined" "x = 10;"

testAssignTypeMismatch :: IO ()
testAssignTypeMismatch = checkErr "assign type mismatch" "x: bool = true; x = 42;"
