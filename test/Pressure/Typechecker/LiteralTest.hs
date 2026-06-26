module Pressure.Typechecker.LiteralTest (literalTypeTests) where

import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

literalTypeTests :: TestTree
literalTypeTests =
  testGroup
    "literals"
    [ testCase "checks int literals" testIntLit,
      testCase "checks float literals" testFloatLit,
      testCase "checks bool literals" testBoolLit,
      testCase "checks string literals" testStringLit
    ]

testIntLit :: IO ()
testIntLit = checkOk "int literal" "x: int = 42;"

testFloatLit :: IO ()
testFloatLit = checkOk "float literal" "x: float = 3.14;"

testBoolLit :: IO ()
testBoolLit = checkOk "bool literal" "x: bool = true;"

testStringLit :: IO ()
testStringLit = checkOk "string literal" "x: string = \"hello\";"
