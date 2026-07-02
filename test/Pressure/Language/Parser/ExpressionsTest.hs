module Pressure.Language.Parser.ExpressionsTest (parserExpressionTests) where

import Pressure.Language.Ast
import Pressure.Language.Lexer (Token (Mod))
import Pressure.Language.Parser.TestUtil
import Pressure.Language.Types (BinaryOp (..), Mutability (..), UnaryOp (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

parserExpressionTests :: TestTree
parserExpressionTests =
  testGroup
    "expressions"
    [ testCase "parses addition expression" testParseAdditionExpr,
      testCase "parses multiplication precedence" testParseMulPrecedence,
      testCase "parses division expression" testParseDivisionExpr,
      testCase "parses mod expression" testParseModExpr,
      testCase "parses parenthesized expression" testParseParenExpr,
      testCase "parses bare expression" testParseBareExpr,
      testCase "parses variable reference" testParseVarRef,
      testCase "parses subtraction precedence" testParseSubPrecedence,
      testCase "parses boolean precedence" testParseBoolPrecedence,
      testCase "parses comparison after arithmetic" testParseComparisonPrecedence,
      testCase "parses unary negation" testParseUnaryNeg,
      testCase "parses unary not" testParseUnaryNot,
      testCase "parses unary ampersand" testParseUnaryAmpersand,
      testCase "parses unary precedence" testParseUnaryPrecedence,
      testCase "parses call expression" testParseCallExpr,
      testCase "parses empty array literal" testParseEmptyArrayLit,
      testCase "parses non-empty array literal" testParseNonEmptyArrayLit,
      testCase "parses array type" testParseArrayType,
      testCase "parses index expression" testParseIndexExpr,
      testCase "parses index on literal" testParseIndexOnLiteral,
      testCase "parses nested array literal" testParseNestedArrayLit,
      testCase "parses array of strings" testParseArrayOfStrings,
      testCase "parses mutable address-of" testParseAddrOfMut,
      testCase "parses deref postfix" testParseDeref,
      testCase "parses pointer type" testParsePointerType,
      testCase "parses mutable pointer type" testParsePointerTypeMut
    ]

testParseAdditionExpr :: IO ()
testParseAdditionExpr = do
  ast <- parse "parse addition expression" "sum: i32 = 1 + 2 + 3;"
  expect "addition expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3))))) -> isIntSyntax typ; _ -> False) ast

testParseMulPrecedence :: IO ()
testParseMulPrecedence = do
  ast <- parse "parse multiplication precedence" "value: i32 = 1 + 2 * 3;"
  expect "multiplication precedence" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3))))))) -> isIntSyntax typ; _ -> False) ast

testParseDivisionExpr :: IO ()
testParseDivisionExpr = do
  ast <- parse "parse division expression" "value: i32 = 8 / 4 / 2;"
  expect "division expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedIntLit 8)) (ParsedExpr _ (ParsedIntLit 4)))) (ParsedExpr _ (ParsedIntLit 2))))) -> isIntSyntax typ; _ -> False) ast

testParseModExpr :: IO ()
testParseModExpr = do
  ast <- parse "parse mod expression" "value: i32 = 8 % 4 % 2;"
  expect "mod expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr ModOp (ParsedExpr _ (ParsedBinaryExpr ModOp (ParsedExpr _ (ParsedIntLit 8)) (ParsedExpr _ (ParsedIntLit 4)))) (ParsedExpr _ (ParsedIntLit 2))))) -> isIntSyntax typ; _ -> False) ast

testParseParenExpr :: IO ()
testParseParenExpr = do
  ast <- parse "parse parenthesized expression" "value: i32 = (1 + 2) * 3;"
  expect "parenthesized expression" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3))))) -> isIntSyntax typ; _ -> False) ast

testParseBareExpr :: IO ()
testParseBareExpr = do
  ast <- parse "parse bare expression" "1 + 2;"
  expect "bare expression" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) -> True; _ -> False) ast

testParseVarRef :: IO ()
testParseVarRef = do
  ast <- parse "parse variable reference as expression" "x;"
  expect "variable reference" (case singleExpr ast of Just (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) -> True; _ -> False) ast

testParseSubPrecedence :: IO ()
testParseSubPrecedence = do
  ast <- parse "parse subtraction precedence" "value: i32 = 1 - 2 * 3;"
  expect "subtraction precedence" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (ParsedExpr _ (ParsedBinaryExpr SubOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3))))))) -> isIntSyntax typ; _ -> False) ast

testParseBoolPrecedence :: IO ()
testParseBoolPrecedence = do
  ast <- parse "parse boolean precedence" "true or false and 1 < 2;"
  expect "boolean precedence" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr OrOp _ _)) -> True; _ -> False) ast

testParseComparisonPrecedence :: IO ()
testParseComparisonPrecedence = do
  ast <- parse "parse comparison after arithmetic" "1 + 2 * 3 == 7;"
  expect "comparison precedence" (case singleExpr ast of Just e -> isBinary EqOp (isBinary AddOp (isIntLit 1) (isBinary MulOp (isIntLit 2) (isIntLit 3))) (isIntLit 7) e; _ -> False) ast

testParseUnaryNeg :: IO ()
testParseUnaryNeg = do
  ast <- parse "parse unary negation" "-1;"
  expect "unary negation" (case singleExpr ast of Just (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 1)))) -> True; _ -> False) ast

testParseUnaryNot :: IO ()
testParseUnaryNot = do
  ast <- parse "parse unary not" "!false;"
  expect "unary not" (case singleExpr ast of Just (ParsedExpr _ (ParsedUnaryExpr NotOp (ParsedExpr _ (ParsedBoolLit False)))) -> True; _ -> False) ast

testParseUnaryAmpersand :: IO ()
testParseUnaryAmpersand = do
  ast <- parse "parse unary ampersand" "&x;"
  expect "unary ampersand" (case singleExpr ast of Just (ParsedExpr _ (ParsedAddrOfExpr False (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))))) -> True; _ -> False) ast

testParseAddrOfMut :: IO ()
testParseAddrOfMut = do
  ast <- parse "parse mutable address-of" "&mut x;"
  expect "mutable address-of" (case singleExpr ast of Just (ParsedExpr _ (ParsedAddrOfExpr True (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))))) -> True; _ -> False) ast

testParseDeref :: IO ()
testParseDeref = do
  ast <- parse "parse deref postfix" "y.*;"
  expect "deref postfix" (case singleExpr ast of Just (ParsedExpr _ (ParsedDerefExpr (ParsedExpr _ (ParsedVarExpr (Ident _ "y"))))) -> True; _ -> False) ast

isPointerType :: Bool -> TypeSyntax -> Bool
isPointerType expectedMut = \case
  TypeSyntax _ (PointerSyntax _ m) -> m == (if expectedMut then Mutable else Constant)
  _ -> False

testParsePointerType :: IO ()
testParsePointerType = do
  ast <- parse "parse pointer type" "x: *int = &y;"
  expect "pointer type" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) _) -> isPointerType False typ; _ -> False) ast

testParsePointerTypeMut :: IO ()
testParsePointerTypeMut = do
  ast <- parse "parse mutable pointer type" "x: *mut int = &mut y;"
  expect "mutable pointer type" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) _) -> isPointerType True typ; _ -> False) ast

testParseUnaryPrecedence :: IO ()
testParseUnaryPrecedence = do
  ast <- parse "parse unary precedence" "1 * -2;"
  expect "unary precedence" (case singleExpr ast of Just (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 2)))))) -> True; _ -> False) ast

testParseCallExpr :: IO ()
testParseCallExpr = do
  ast <- parse "parse call expression" "add(1, 2);"
  expect "call expression" (case singleExpr ast of Just (ParsedExpr _ (ParsedCallExpr (ParsedExpr _ (ParsedVarExpr (Ident _ "add"))) [ParsedExpr _ (ParsedIntLit 1), ParsedExpr _ (ParsedIntLit 2)])) -> True; _ -> False) ast

isArrayLit :: [ParsedExpr -> Bool] -> ParsedExpr -> Bool
isArrayLit matchers = \case
  ParsedExpr _ (ParsedArrayLit elems) ->
    length matchers == length elems
      && and (zipWith (\m e -> m e) matchers elems)
  _ -> False

isArrayLitOf :: [Integer] -> ParsedExpr -> Bool
isArrayLitOf expected = \case
  ParsedExpr _ (ParsedArrayLit elems) ->
    length expected == length elems
      && and (zipWith (\(ParsedExpr _ (ParsedIntLit e)) n -> e == n) elems expected)
  _ -> False

isIndexExpr :: (ParsedExpr -> Bool) -> (ParsedExpr -> Bool) -> ParsedExpr -> Bool
isIndexExpr base index = \case
  ParsedExpr _ (ParsedIndexExpr b i) -> base b && index i
  _ -> False

isStringLit :: String -> ParsedExpr -> Bool
isStringLit expected = \case
  ParsedExpr _ (ParsedStringLit actual) -> expected == actual
  _ -> False

isArrayType :: (TypeSyntax -> Bool) -> TypeSyntax -> Bool
isArrayType matchInner (TypeSyntax _ (ArraySyntax inner)) = matchInner inner
isArrayType _ _ = False

isIntType :: TypeSyntax -> Bool
isIntType (TypeSyntax _ (IntSyntax _ _)) = True
isIntType _ = False

testParseEmptyArrayLit :: IO ()
testParseEmptyArrayLit = do
  ast <- parse "parse empty array literal" "[];"
  expect "empty array literal" (case singleExpr ast of Just e -> isArrayLit [] e; _ -> False) ast

testParseNonEmptyArrayLit :: IO ()
testParseNonEmptyArrayLit = do
  ast <- parse "parse non-empty array literal" "[1, 2, 3];"
  expect "non-empty array literal" (case singleExpr ast of Just e -> isArrayLitOf [1, 2, 3] e; _ -> False) ast

testParseArrayType :: IO ()
testParseArrayType = do
  ast <- parse "parse array type" "x: []i32 = [];"
  expect
    "array type"
    ( case singleDecl ast of
        Just (ParsedValueDecl Mutable _ (Just typ) _) -> isArrayType isIntType typ
        _ -> False
    )
    ast

testParseIndexExpr :: IO ()
testParseIndexExpr = do
  ast <- parse "parse index expression" "arr[0];"
  expect
    "index expression"
    ( case singleExpr ast of
        Just e ->
          isIndexExpr
            ( \b -> case b of
                ParsedExpr _ (ParsedVarExpr (Ident _ "arr")) -> True
                _ -> False
            )
            (isIntLit 0)
            e
        _ -> False
    )
    ast

testParseIndexOnLiteral :: IO ()
testParseIndexOnLiteral = do
  ast <- parse "parse index on literal" "[1, 2, 3][1];"
  expect
    "index on literal"
    ( case singleExpr ast of
        Just e ->
          isIndexExpr
            (isArrayLitOf [1, 2, 3])
            (isIntLit 1)
            e
        _ -> False
    )
    ast

testParseNestedArrayLit :: IO ()
testParseNestedArrayLit = do
  ast <- parse "parse nested array literal" "[[1, 2], [3, 4]];"
  expect
    "nested array literal"
    ( case singleExpr ast of
        Just e ->
          isArrayLit [isArrayLitOf [1, 2], isArrayLitOf [3, 4]] e
        _ -> False
    )
    ast

testParseArrayOfStrings :: IO ()
testParseArrayOfStrings = do
  ast <- parse "parse array of strings" "[\"a\", \"b\"];"
  expect
    "array of strings"
    ( case singleExpr ast of
        Just e ->
          isArrayLit [isStringLit "a", isStringLit "b"] e
        _ -> False
    )
    ast
