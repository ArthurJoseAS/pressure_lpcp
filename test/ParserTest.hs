module ParserTest (testParser) where

import Ast
import Lexer (tokenizeEither)
import Parser (parseProgram)

assertRight :: String -> Either String a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ err
assertRight _ (Right x) = return x

testParser :: IO ()
testParser = do
  tokens <-
    assertRight "tokenize mutable declaration" $
      tokenizeEither "x: i32 = 42;"
  let ast = parseProgram tokens
  case ast of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) (Just (IntLit _ _)))] -> return ()
    other -> error $ "unexpected AST for mutable declaration: " ++ show other

  tokens2 <-
    assertRight "tokenize constant declaration" $
      tokenizeEither "y: i32 : 7;"
  let ast2 = parseProgram tokens2
  case ast2 of
    Program [TopLevelDecl (ValueDecl Constant _ (Just (TypeName _)) (Just (IntLit _ _)))] -> return ()
    other -> error $ "unexpected AST for constant declaration: " ++ show other

  tokens3 <-
    assertRight "tokenize declaration without value" $
      tokenizeEither "z: i32;"
  let ast3 = parseProgram tokens3
  case ast3 of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) Nothing)] -> return ()
    other -> error $ "unexpected AST for declaration without value: " ++ show other

  tokens4 <-
    assertRight "tokenize addition expression" $
      tokenizeEither "sum: i32 = 1 + 2 + 3;"
  let ast4 = parseProgram tokens4
  case ast4 of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ AddOp (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) (IntLit _ 3))))] -> return ()
    other -> error $ "unexpected AST for addition expression: " ++ show other

  tokens5 <-
    assertRight "tokenize multiplication precedence" $
      tokenizeEither "value: i32 = 1 + 2 * 3;"
  let ast5 = parseProgram tokens5
  case ast5 of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ AddOp (IntLit _ 1) (BinaryExpr _ MulOp (IntLit _ 2) (IntLit _ 3)))))] -> return ()
    other -> error $ "unexpected AST for multiplication precedence: " ++ show other

  tokens6 <-
    assertRight "tokenize division expression" $
      tokenizeEither "value: i32 = 8 / 4 / 2;"
  let ast6 = parseProgram tokens6
  case ast6 of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ DivOp (BinaryExpr _ DivOp (IntLit _ 8) (IntLit _ 4)) (IntLit _ 2))))] -> return ()
    other -> error $ "unexpected AST for division expression: " ++ show other

  tokens7 <-
    assertRight "tokenize parenthesized expression" $
      tokenizeEither "value: i32 = (1 + 2) * 3;"
  let ast7 = parseProgram tokens7
  case ast7 of
    Program [TopLevelDecl (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ MulOp (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) (IntLit _ 3))))] -> return ()
    other -> error $ "unexpected AST for parenthesized expression: " ++ show other
