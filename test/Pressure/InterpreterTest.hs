module Pressure.InterpreterTest (interpreterTests) where

import Pressure.Interpreter.ArithTest (arithTests)
import Pressure.Interpreter.ArrayTest (arrayTests)
import Pressure.Interpreter.AssignTest (assignTests)
import Pressure.Interpreter.ControlTest (controlTests)
import Pressure.Interpreter.ErrorTest (errorTests)
import Pressure.Interpreter.FunctionTest (functionTests)
import Pressure.Interpreter.LiteralTest (literalTests)
import Pressure.Interpreter.ProgramTest (programTests)
import Test.Tasty (TestTree, testGroup)

interpreterTests :: TestTree
interpreterTests =
  testGroup
    "interpreter"
    [ literalTests,
      arithTests,
      assignTests,
      errorTests,
      controlTests,
      functionTests,
      programTests,
      arrayTests
    ]
