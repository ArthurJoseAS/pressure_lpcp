module Pressure.Typechecker.ArrayTest (arrayTypeTests) where

import Control.Monad (void)
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseProgram)
import Pressure.TestUtil (assertLeft, assertOk)
import Pressure.Typechecker (checkProgram)
import Pressure.Typechecker.Error (Error)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

arrayTypeTests :: TestTree
arrayTypeTests =
  testGroup
    "arrays"
    [ testCase "checks array literal and element types" testArrayTypes,
      testCase "checks array indexing" testArrayIndexing
    ]

checkSource :: String -> Either Error ()
checkSource source = case runAlex fullSource parseProgram of
  Left err -> error $ "parse failed: " ++ err
  Right ast -> void (checkProgram ast)
  where
    fullSource =
      if "main" `elem` words (map (\c -> if c `elem` "():;,{}[]" then ' ' else c) source)
        then source
        else source ++ " main :: fn() {};"

testArrayTypes :: IO ()
testArrayTypes = do
  assertOk "uniform int array literal type checks" $
    checkSource "x: []i32 = [1, 2, 3];"

  assertOk "empty array literal with anytype type checks" $
    checkSource "x: []anytype = [];"

  assertOk
    "empty array literal with concrete element type checks (current behavior: anytype is compatible with any other type)"
    $ checkSource "x: []i32 = [];"

  assertOk "nested array literal type checks" $
    checkSource "x: [][]i32 = [[1, 2], [3, 4]];"

  assertOk "bool array literal type checks" $
    checkSource "x: []bool = [true, false, true];"

  assertOk "string array literal type checks" $
    checkSource "x: []string = [\"a\", \"b\"];"

  assertOk "float array literal type checks" $
    checkSource "x: []f64 = [1.0, 2.0];"

  assertOk "[]T works as function parameter and return type" $
    checkSource "f :: fn(a: []i32) -> []i32 { a };"

  assertOk "array type annotation is respected" $
    checkSource "x: []i32 = [1, 2, 3]; y: i32 = x[0];"

  assertLeft "mixed element types in int array rejected" $
    checkSource "x: []i32 = [1, true];"

  assertLeft "non-array assigned to array variable rejected" $
    checkSource "x: []i32 = 42;"

  assertLeft "array type annotation mismatches element type rejected" $
    checkSource "x: []i32 = [1.0, 2.0];"

  assertLeft "nested array element type mismatch rejected" $
    checkSource "x: [][]i32 = [[1, 2], [3.0]];"

  assertLeft "function returning non-array from declared []T rejected" $
    checkSource "f :: fn() -> []i32 { 42 };"

testArrayIndexing :: IO ()
testArrayIndexing = do
  assertOk "indexing int array with int literal returns int" $
    checkSource "x: []i32 = [10, 20, 30]; y: i32 = x[1];"

  assertOk "indexing with a variable index type checks" $
    checkSource "x: []i32 = [10, 20, 30]; i: i32 = 0; y: i32 = x[i];"

  assertOk "indexing a nested array type checks" $
    checkSource "mat: [][]i32 = [[1, 2], [3, 4]]; y: i32 = mat[0][1];"

  assertOk "indexing yields the element type (string)" $
    checkSource "xs: []string = [\"a\", \"b\"]; y: string = xs[0];"

  assertLeft "indexing int array with non-int rejected" $
    checkSource "x: []i32 = [1, 2, 3]; y: i32 = x[true];"

  assertLeft "indexing a non-array value rejected" $
    checkSource "x: i32 = 5; y: i32 = x[0];"

  assertLeft "indexing with a float index rejected" $
    checkSource "x: []i32 = [1, 2, 3]; y: i32 = x[0.0];"
