module ParserTest (testParser) where

import Ast
import Lexer (runAlex)
import Parser (parseProgram, parseRepl)

assertRight :: String -> Either String a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ err
assertRight _ (Right x) = return x

testParser :: IO ()
testParser = do
  testParseProgram
  testParseRepl
  testParseErrors

testParseProgram :: IO ()
testParseProgram = do
  ast <-
    assertRight "parse mutable declaration" $
      runAlex "x: i32 = 42;" parseProgram
  case ast of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (IntLit _ _))))] -> return ()
    other -> error $ "unexpected AST for mutable declaration: " ++ show other

  ast2 <-
    assertRight "parse constant declaration" $
      runAlex "y: i32 : 7;" parseProgram
  case ast2 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Constant _ (Just (TypeName _)) (Just (IntLit _ _))))] -> return ()
    other -> error $ "unexpected AST for constant declaration: " ++ show other

  ast4 <-
    assertRight "parse addition expression" $
      runAlex "sum: i32 = 1 + 2 + 3;" parseProgram
  case ast4 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ AddOp (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) (IntLit _ 3)))))] -> return ()
    other -> error $ "unexpected AST for addition expression: " ++ show other

  ast5 <-
    assertRight "parse multiplication precedence" $
      runAlex "value: i32 = 1 + 2 * 3;" parseProgram
  case ast5 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ AddOp (IntLit _ 1) (BinaryExpr _ MulOp (IntLit _ 2) (IntLit _ 3))))))] -> return ()
    other -> error $ "unexpected AST for multiplication precedence: " ++ show other

  ast6 <-
    assertRight "parse division expression" $
      runAlex "value: i32 = 8 / 4 / 2;" parseProgram
  case ast6 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ DivOp (BinaryExpr _ DivOp (IntLit _ 8) (IntLit _ 4)) (IntLit _ 2)))))] -> return ()
    other -> error $ "unexpected AST for division expression: " ++ show other

  ast7 <-
    assertRight "parse parenthesized expression" $
      runAlex "value: i32 = (1 + 2) * 3;" parseProgram
  case ast7 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ MulOp (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) (IntLit _ 3)))))] -> return ()
    other -> error $ "unexpected AST for parenthesized expression: " ++ show other

  ast8 <-
    assertRight "parse bare expression as program" $
      runAlex "1 + 2;" parseProgram
  case ast8 of
    Program [TopLevelStmt (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2))] -> return ()
    other -> error $ "unexpected AST for bare expression: " ++ show other

  ast9 <-
    assertRight "parse variable reference as expression" $
      runAlex "x;" parseProgram
  case ast9 of
    Program [TopLevelStmt (VarExpr _ (Ident _ "x"))] -> return ()
    other -> error $ "unexpected AST for variable reference: " ++ show other

  ast10 <-
    assertRight "parse subtraction precedence" $
      runAlex "value: i32 = 1 - 2 * 3;" parseProgram
  case ast10 of
    Program [TopLevelStmt (DeclExpr _ (ValueDecl Mutable _ (Just (TypeName _)) (Just (BinaryExpr _ SubOp (IntLit _ 1) (BinaryExpr _ MulOp (IntLit _ 2) (IntLit _ 3))))))] -> return ()
    other -> error $ "unexpected AST for subtraction precedence: " ++ show other

testParseRepl :: IO ()
testParseRepl = do
  ast <-
    assertRight "repl: declaration without semicolon" $
      runAlex "x: int = 42" parseRepl
  case ast of
    ReplExpr (DeclExpr _ (ValueDecl Mutable _ (Just (IntType _)) (Just (IntLit _ 42)))) -> return ()
    other -> error $ "unexpected AST for repl declaration: " ++ show other

  ast2 <-
    assertRight "repl: declaration with semicolon" $
      runAlex "x: int = 42;" parseRepl
  case ast2 of
    ReplStmt (DeclExpr _ (ValueDecl Mutable _ (Just (IntType _)) (Just (IntLit _ 42)))) -> return ()
    other -> error $ "unexpected AST for repl declaration stmt: " ++ show other

  ast3 <-
    assertRight "repl: bare expression" $
      runAlex "1 + 2" parseRepl
  case ast3 of
    ReplExpr (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) -> return ()
    other -> error $ "unexpected AST for repl expression: " ++ show other

  ast4 <-
    assertRight "repl: expression with semicolon" $
      runAlex "1 + 2;" parseRepl
  case ast4 of
    ReplStmt (BinaryExpr _ AddOp (IntLit _ 1) (IntLit _ 2)) -> return ()
    other -> error $ "unexpected AST for repl expression stmt: " ++ show other

  ast5 <-
    assertRight "repl: variable reference" $
      runAlex "x" parseRepl
  case ast5 of
    ReplExpr (VarExpr _ (Ident _ "x")) -> return ()
    other -> error $ "unexpected AST for repl variable reference: " ++ show other

  ast6 <-
    assertRight "repl: variable in expression" $
      runAlex "x + 1" parseRepl
  case ast6 of
    ReplExpr (BinaryExpr _ AddOp (VarExpr _ (Ident _ "x")) (IntLit _ 1)) -> return ()
    other -> error $ "unexpected AST for repl variable in expr: " ++ show other

testParseErrors :: IO ()
testParseErrors = do
  assertLeft "program requires semicolon" $ runAlex "1 + 2" parseProgram
  assertLeft "malformed expression" $ runAlex "x: int = 1 + ;" parseProgram

assertLeft :: String -> Either String a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected parse error"
