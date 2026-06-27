module Main (main) where

import Pressure.BuiltinsTest (builtinsTests)
import Pressure.InterpreterTest (interpreterTests)
import Pressure.Language.LexerTest (lexerTests)
import Pressure.Language.ParserTest (parserTests)
import Pressure.TypecheckerTest (typeTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pressure-lang"
      [ lexerTests,
        parserTests,
        typeTests,
        interpreterTests,
        builtinsTests
      ]
