module Main (main) where

import AstTest (astTests)
import LexerTest (lexerTests)
import ParserTest (parserTests)
import Test.Tasty (defaultMain, testGroup)
import TypeTest (typeTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pressure-lang"
      [ lexerTests,
        parserTests,
        typeTests,
        astTests
      ]
