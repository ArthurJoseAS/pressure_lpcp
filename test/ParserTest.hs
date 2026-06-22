module ParserTest (testParser) where

import Parser.ErrorTest (testParseErrorFormat, testParseErrors)
import Parser.ProgramTest (testParseProgram)
import Parser.ReplTest (testParseRepl)

testParser :: IO ()
testParser = do
  testParseProgram
  testParseRepl
  testParseErrors
  testParseErrorFormat
