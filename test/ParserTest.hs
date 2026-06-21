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
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit _))))))] -> return ()
    other -> error $ "unexpected AST for mutable declaration: " ++ show other

  ast2 <-
    assertRight "parse constant declaration" $
      runAlex "y: i32 : 7;" parseProgram
  case ast2 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Constant _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit _))))))] -> return ()
    other -> error $ "unexpected AST for constant declaration: " ++ show other

  ast4 <-
    assertRight "parse addition expression" $
      runAlex "sum: i32 = 1 + 2 + 3;" parseProgram
  case ast4 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr AddOp (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) (Expr _ (IntLit 3))))))))] -> return ()
    other -> error $ "unexpected AST for addition expression: " ++ show other

  ast5 <-
    assertRight "parse multiplication precedence" $
      runAlex "value: i32 = 1 + 2 * 3;" parseProgram
  case ast5 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))))))))] -> return ()
    other -> error $ "unexpected AST for multiplication precedence: " ++ show other

  ast6 <-
    assertRight "parse division expression" $
      runAlex "value: i32 = 8 / 4 / 2;" parseProgram
  case ast6 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr DivOp (Expr _ (BinaryExpr DivOp (Expr _ (IntLit 8)) (Expr _ (IntLit 4)))) (Expr _ (IntLit 2))))))))] -> return ()
    other -> error $ "unexpected AST for division expression: " ++ show other

  ast7 <-
    assertRight "parse parenthesized expression" $
      runAlex "value: i32 = (1 + 2) * 3;" parseProgram
  case ast7 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr MulOp (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) (Expr _ (IntLit 3))))))))] -> return ()
    other -> error $ "unexpected AST for parenthesized expression: " ++ show other

  ast8 <-
    assertRight "parse bare expression as program" $
      runAlex "1 + 2;" parseProgram
  case ast8 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2))))))] -> return ()
    other -> error $ "unexpected AST for bare expression: " ++ show other

  ast9 <-
    assertRight "parse variable reference as expression" $
      runAlex "x;" parseProgram
  case ast9 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (VarExpr (Ident _ "x")))))] -> return ()
    other -> error $ "unexpected AST for variable reference: " ++ show other

  ast10 <-
    assertRight "parse subtraction precedence" $
      runAlex "value: i32 = 1 - 2 * 3;" parseProgram
  case ast10 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr SubOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))))))))] -> return ()
    other -> error $ "unexpected AST for subtraction precedence: " ++ show other

  ast11 <-
    assertRight "parse boolean precedence" $
      runAlex "true or false and 1 < 2;" parseProgram
  case ast11 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr OrOp _ _))))] -> return ()
    other -> error $ "unexpected AST for boolean precedence: " ++ show other

  ast12 <-
    assertRight "parse comparison after arithmetic" $
      runAlex "1 + 2 * 3 == 7;" parseProgram
  case ast12 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr EqOp l r))))] ->
      case (l, r) of
        (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))), Expr _ (IntLit 7)) -> return ()
        _ -> error $ "unexpected AST for comparison precedence: " ++ show ast12
    other -> error $ "unexpected AST for comparison precedence: " ++ show other

  ast13 <-
    assertRight "parse if expression with else" $
      runAlex "x: int = if true { 1 } else { 2 };" parseProgram
  case ast13 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IfExpr _ _ (Just _)))))))] -> return ()
    other -> error $ "unexpected AST for if expression: " ++ show other

  ast14 <-
    assertRight "parse if statement without else" $
      runAlex "if true { x: int = 1; }" parseProgram
  case ast14 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (IfExpr _ _ Nothing))))] -> return ()
    other -> error $ "unexpected AST for if statement: " ++ show other

testParseRepl :: IO ()
testParseRepl = do
  ast <-
    assertRight "repl: declaration without semicolon" $
      runAlex "x: int = 42" parseRepl
  case ast of
    ReplStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit 42)))))) -> return ()
    other -> error $ "unexpected AST for repl declaration: " ++ show other

  ast2 <-
    assertRight "repl: declaration with semicolon" $
      runAlex "x: int = 42;" parseRepl
  case ast2 of
    ReplStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit 42)))))) -> return ()
    other -> error $ "unexpected AST for repl declaration stmt: " ++ show other

  ast3 <-
    assertRight "repl: bare expression" $
      runAlex "1 + 2" parseRepl
  case ast3 of
    ReplExpr (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) -> return ()
    other -> error $ "unexpected AST for repl expression: " ++ show other

  ast4 <-
    assertRight "repl: expression with semicolon" $
      runAlex "1 + 2;" parseRepl
  case ast4 of
    ReplStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))))) -> return ()
    other -> error $ "unexpected AST for repl expression stmt: " ++ show other

  ast5 <-
    assertRight "repl: variable reference" $
      runAlex "x" parseRepl
  case ast5 of
    ReplExpr (Expr _ (VarExpr (Ident _ "x"))) -> return ()
    other -> error $ "unexpected AST for repl variable reference: " ++ show other

  ast6 <-
    assertRight "repl: variable in expression" $
      runAlex "x + 1" parseRepl
  case ast6 of
    ReplExpr (Expr _ (BinaryExpr AddOp (Expr _ (VarExpr (Ident _ "x"))) (Expr _ (IntLit 1)))) -> return ()
    other -> error $ "unexpected AST for repl variable in expr: " ++ show other

testParseErrors :: IO ()
testParseErrors = do
  assertLeft "program requires semicolon" $ runAlex "1 + 2" parseProgram
  assertLeft "malformed expression" $ runAlex "x: int = 1 + ;" parseProgram
  assertLeft "chained comparisons are forbidden" $ runAlex "1 < 2 < 3;" parseProgram
  _ <- assertRight "if expression without else parses" $ runAlex "x: int = if true { 1 };" parseProgram
  return ()

assertLeft :: String -> Either String a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected parse error"
