module ParserTest (parserTests) where

import Parser.ErrorTest (parserErrorTests)
import Parser.ProgramTest (parserProgramTests)
import Parser.ReplTest (parserReplTests)
import Test.Tasty (TestTree, testGroup)

parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [ parserProgramTests,
      parserReplTests,
      parserErrorTests
    ]
