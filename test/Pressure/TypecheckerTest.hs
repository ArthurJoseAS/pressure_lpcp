module Pressure.TypecheckerTest (typeTests) where

import Pressure.Typechecker.ArithTest (arithTypeTests)
import Pressure.Typechecker.ArrayTest (arrayTypeTests)
import Pressure.Typechecker.AssignTest (assignTypeTests)
import Pressure.Typechecker.ControlTest (controlTypeTests)
import Pressure.Typechecker.ErrorTest (errorTypeTests)
import Pressure.Typechecker.FunctionTest (functionTypeTests)
import Pressure.Typechecker.LiteralTest (literalTypeTests)
import Pressure.Typechecker.PointerTest (pointerTypeTests)
import Pressure.Typechecker.StructTest (structTypeTests)
import Test.Tasty (TestTree, testGroup)

typeTests :: TestTree
typeTests =
  testGroup
    "types"
    [ literalTypeTests,
      arithTypeTests,
      assignTypeTests,
      errorTypeTests,
      controlTypeTests,
      functionTypeTests,
      structTypeTests,
      arrayTypeTests,
      pointerTypeTests
    ]
