module AstTest (testAst) where

import Ast
import Control.Monad.State (runStateT)
import qualified Data.Map.Strict as Map
import Lexer (AlexPosn (..), runAlex)
import Parser (parseProgram)
import TypeCheck (checkProgram)

assertRight :: String -> Either String a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ err
assertRight _ (Right x) = return x

assertExpr :: String -> Expr -> Env -> Value -> IO ()
assertExpr name expr env expected = do
  case runStateT (evalExpr expr) env of
    Left err -> error $ name ++ " failed: " ++ err
    Right (val, _) ->
      if val == expected
        then return ()
        else error $ name ++ ": expected " ++ show expected ++ " but got " ++ show val

assertEvalError :: String -> Expr -> Env -> String -> IO ()
assertEvalError name expr env expectedErr = do
  case runStateT (evalExpr expr) env of
    Left err ->
      if err == expectedErr
        then return ()
        else error $ name ++ ": expected error '" ++ expectedErr ++ "' but got '" ++ err ++ "'"
    Right (val, _) -> error $ name ++ ": expected error but got " ++ show val

testAst :: IO ()
testAst = do
  testIntLit
  testFloatLit
  testBoolLit
  testIntAdd
  testFloatAdd
  testMixedAdd
  testIntPromotion
  testIntDiv
  testFloatDiv
  testDivByZero
  testTypeNameAnnotation
  testBoolInArithmeticError
  testBoolInArithmeticRightError
  testTypeMismatchError
  testFloatNarrowingError
  testVarDeclAndLookup
  testVarUndefined
  testVarDefaultValue
  testIntMul
  testFloatMul

withTokens :: String -> String -> (Program -> IO ()) -> IO ()
withTokens name source f = do
  ast <- assertRight ("parse " ++ name) $ runAlex source parseProgram
  f ast

checkOk :: String -> String -> IO ()
checkOk name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Right () -> return ()
      Left err -> error $ name ++ " failed: " ++ show err

checkErr :: String -> String -> IO ()
checkErr name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Left _ -> return ()
      Right () -> error $ name ++ ": expected type error but passed"

testIntLit :: IO ()
testIntLit = do
  checkOk "int literal" "x: int = 42;"
  withTokens "int literal eval" "x: int = 42;" $ \ast ->
    case runStateT (evalProgram ast) Map.empty of
      Left err -> error $ "int literal eval failed: " ++ err
      Right (val, env) -> do
        if val == VUnit then return () else error $ "expected VUnit got " ++ show val
        case Map.lookup "x" env of
          Just (VInt 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other

testFloatLit :: IO ()
testFloatLit = do
  checkOk "float literal" "x: float = 3.14;"
  withTokens "float literal eval" "x: float = 3.14;" $ \ast ->
    case runStateT (evalProgram ast) Map.empty of
      Left err -> error $ "float literal eval failed: " ++ err
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VFloat 3.14) -> return ()
          other -> error $ "expected 3.14, got " ++ show other

testBoolLit :: IO ()
testBoolLit = do
  checkOk "bool literal" "x: bool = true;"
  withTokens "bool literal eval" "x: bool = true;" $ \ast ->
    case runStateT (evalProgram ast) Map.empty of
      Left err -> error $ "bool literal eval failed: " ++ err
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected true, got " ++ show other

testIntAdd :: IO ()
testIntAdd = checkOk "int addition" "x: int = 1 + 2;"

testFloatAdd :: IO ()
testFloatAdd = checkOk "float addition" "x: float = 1.0 + 2.0;"

testMixedAdd :: IO ()
testMixedAdd = checkOk "mixed int+float" "x: float = 1 + 2.0;"

testIntPromotion :: IO ()
testIntPromotion = do
  checkOk "int promotion to float annotation" "x: float = 1 + 2;"

testIntDiv :: IO ()
testIntDiv = checkOk "int division" "x: int = 8 / 4;"

testFloatDiv :: IO ()
testFloatDiv = checkOk "float division" "x: float = 3.0 / 2.0;"

testDivByZero :: IO ()
testDivByZero = do
  withTokens "division by zero int" "x: int = 1 / 0;" $ \ast ->
    case checkProgram ast of
      Right () ->
        case runStateT (evalProgram ast) Map.empty of
          Left err ->
            if err == "division by zero"
              then return ()
              else error $ "expected 'division by zero' got '" ++ err ++ "'"
          Right _ -> error "expected runtime error for division by zero"
      Left _ -> return ()

  withTokens "division by zero float" "x: float = 1.0 / 0.0;" $ \ast ->
    case checkProgram ast of
      Right () ->
        case runStateT (evalProgram ast) Map.empty of
          Left err ->
            if err == "division by zero"
              then return ()
              else error $ "expected 'division by zero' got '" ++ err ++ "'"
          Right _ -> error "expected runtime error for division by zero"
      Left _ -> return ()

testTypeNameAnnotation :: IO ()
testTypeNameAnnotation = checkOk "TypeName annotation" "x: i32 = 42;"

testBoolInArithmeticError :: IO ()
testBoolInArithmeticError = checkErr "bool in arithmetic" "x: int = true + 1;"

testBoolInArithmeticRightError :: IO ()
testBoolInArithmeticRightError = checkErr "bool on right of arithmetic" "x: int = 1 + true;"

testTypeMismatchError :: IO ()
testTypeMismatchError = checkErr "type mismatch" "x: bool = 42;"

testFloatNarrowingError :: IO ()
testFloatNarrowingError = checkErr "float to int narrowing" "x: int = 3.14;"

testVarDeclAndLookup :: IO ()
testVarDeclAndLookup = do
  let decl42 = "x: int = 42;"
  withTokens "parse decl42" decl42 $ \ast ->
    case checkProgram ast of
      Right () -> do
        case runStateT (evalProgram ast) Map.empty of
          Right (_, env) ->
            assertExpr "x after decl" (VarExpr pos0 (identFrom "x")) env (VInt 42)
          Left err -> error $ "eval failed: " ++ err
      Left err -> error $ "type check failed: " ++ show err

testVarUndefined :: IO ()
testVarUndefined = do
  assertEvalError "undefined variable" (VarExpr pos0 (identFrom "z")) Map.empty "undefined variable: z"

testVarDefaultValue :: IO ()
testVarDefaultValue = do
  withTokens "decl without init" "x: int;" $ \ast ->
    case checkProgram ast of
      Right () -> do
        case runStateT (evalProgram ast) Map.empty of
          Right (_, env) ->
            case Map.lookup "x" env of
              Just (VInt 0) -> return ()
              other -> error $ "expected x = 0 for uninitialized int, got " ++ show other
          Left err -> error $ "eval failed: " ++ err
      Left err -> error $ "type check failed: " ++ show err

testIntMul :: IO ()
testIntMul = checkOk "int multiplication" "x: int = 3 * 4;"

testFloatMul :: IO ()
testFloatMul = checkOk "float multiplication" "x: float = 1.5 * 2.0;"

pos0 :: Lexer.AlexPosn
pos0 = Lexer.AlexPn 0 1 1

identFrom :: String -> Ident
identFrom name = Ident pos0 name
