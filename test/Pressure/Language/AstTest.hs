module Pressure.Language.AstTest (astTests) where

import Pressure.Language.Ast.ArithTest (arithTests)
import Pressure.Language.Ast.AssignTest (assignTests)
import Pressure.Language.Ast.ControlTest (controlTests)
import Pressure.Language.Ast.ErrorTest (errorTests)
import Pressure.Language.Ast.FunctionTest (functionTests)
import Pressure.Language.Ast.LiteralTest (literalTests)
import Test.Tasty (TestTree, testGroup)

astTests :: TestTree
astTests =
  testGroup
    "ast"
    [ literalTests,
      arithTests,
      assignTests,
      errorTests,
      controlTests,
      functionTests
    ]
