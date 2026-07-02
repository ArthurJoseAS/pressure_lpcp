module Pressure.Typechecker.PointerTest (pointerTypeTests) where

import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

pointerTypeTests :: TestTree
pointerTypeTests =
  testGroup
    "pointers"
    [ testCase "infers *T for &x" testInferAddrOfImmutable,
      testCase "infers *mut T for &mut x" testInferAddrOfMut,
      testCase "rejects &mut on a Constant l-value" testRejectMutAddrOfConstant,
      testCase "rejects & on a non-l-value" testRejectAddrOfNonLValue,
      testCase "dereference yields inner type" testDerefYieldsInner,
      testCase "rejects dereference of non-pointer" testRejectDerefNonPointer,
      testCase "rejects assignment through *T" testRejectAssignThroughImmutablePtr,
      testCase "accepts assignment through *mut T" testAcceptAssignThroughMutPtr,
      testCase "supports &s.x on struct fields" testAddrOfStructField,
      testCase "rejects taking address of a function parameter as &mut" testRejectMutAddrOfParam,
      testCase "accepts function parameter of pointer type" testPointerParam
    ]

testInferAddrOfImmutable :: IO ()
testInferAddrOfImmutable = checkOk "&x infers *T" "x : int = 1; y : *int : &x;"

testInferAddrOfMut :: IO ()
testInferAddrOfMut = checkOk "&mut x infers *mut T" "x : int = 1; y : *mut int : &mut x;"

testRejectMutAddrOfConstant :: IO ()
testRejectMutAddrOfConstant = checkErr "&mut of constant" "x : int : 1; y : *mut int : &mut x;"

testRejectAddrOfNonLValue :: IO ()
testRejectAddrOfNonLValue = checkErr "& on non-l-value" "y : *int : &(1 + 2);"

testDerefYieldsInner :: IO ()
testDerefYieldsInner = checkOk "deref yields inner type" "x : int = 1; p : *int = &x; v : int = p.*;"

testRejectDerefNonPointer :: IO ()
testRejectDerefNonPointer = checkErr "deref of non-pointer" "x : int = 1; v : int = x.*;"

testRejectAssignThroughImmutablePtr :: IO ()
testRejectAssignThroughImmutablePtr =
  checkErr "assign through *T rejected" "x : int = 1; p : *int = &x; p.* = 2;"

testAcceptAssignThroughMutPtr :: IO ()
testAcceptAssignThroughMutPtr =
  checkOk "assign through *mut T accepted" "x : int = 1; p : *mut int = &mut x; p.* = 2;"

testAddrOfStructField :: IO ()
testAddrOfStructField =
  checkOk "address of struct field" "S :: struct { v : int }; s : S : .{ v = 1 }; p : *int = &s.v;"

testRejectMutAddrOfParam :: IO ()
testRejectMutAddrOfParam =
  checkErr
    "&mut on function parameter rejected"
    "f :: fn(x: int) { y : *mut int = &mut x; }; main :: fn() { f(1); };"

testPointerParam :: IO ()
testPointerParam =
  checkOk
    "function parameter of pointer type"
    "f :: fn(p: *mut int) { p.* = 2; }; x : int = 1; main :: fn() { f(&mut x); };"
