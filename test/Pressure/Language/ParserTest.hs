module Pressure.Language.ParserTest (parserTests) where

import Pressure.Language.Parser.ErrorTest (parserErrorTests)
import Pressure.Language.Parser.ExpressionsTest (parserExpressionTests)
import Pressure.Language.Parser.ProgramTest (parserProgramTests)
import Pressure.Language.Parser.ReplTest (parserReplTests)
import Test.Tasty (TestTree, testGroup)

parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [ parserProgramTests,
      parserExpressionTests,
      parserReplTests,
      parserErrorTests
    ]
