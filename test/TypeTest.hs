module TypeTest (testType) where

import Ast
import Lexer (AlexPosn (..))
import Type

testType :: IO ()
testType = do
  assertEqual "int addition type" (Right (IntType pos0)) $ typeOf (BinaryExpr pos0 AddOp (IntLit pos0 1) (IntLit pos0 2))
  assertEqual "mixed addition promotes to float" (Right (FloatType pos0)) $ typeOf (BinaryExpr pos0 AddOp (IntLit pos0 1) (FloatLit pos0 2.0))
  assertLeft "bool arithmetic unsupported" $ typeOf (BinaryExpr pos0 AddOp (BoolLit pos0 True) (IntLit pos0 1))
  assertLeft "missing annotation" $ checkProgram (Program [TopLevelStmt (DeclExpr pos0 (ValueDecl Mutable (identFrom "x") Nothing Nothing))])
  assertEqual "repl expression type checks" (Right ()) $ checkReplInput (ReplExpr (IntLit pos0 1))

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then return ()
    else error $ name ++ " failed:\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual

assertLeft :: String -> Either e a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected type error"

pos0 :: AlexPosn
pos0 = AlexPn 0 1 1

identFrom :: String -> Ident
identFrom = Ident pos0
