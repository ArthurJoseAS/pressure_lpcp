module Pressure.TestUtil
  ( assertEqual,
    assertRight,
    assertLeft,
    assertOk,
    assertExpr,
    assertEvalError,
    Error (..),
    RuntimeError (..),
    pos0,
    identFrom,
    emptyEnv,
    lookupValue,
    withTokens,
    checkOk,
    checkErr,
    evalParsed,
    evalProgramFromSource,
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Data.IORef
import Data.Map.Strict qualified as Map
import System.IO.Unsafe (unsafePerformIO)
import Pressure.Builtins (initialValueEnv)
import Pressure.Interpreter.Env (Env)
import Pressure.Interpreter.Error (Error (..), EvalError, RuntimeError (..))
import Pressure.Interpreter.Eval (evalExpr, evalProgram, evalRepl)
import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Ast
import Pressure.Language.Lexer (AlexPosn (..), runAlex)
import Pressure.Language.Parser (genAst, parseProgram, parseRepl)
import Pressure.Typechecker (checkProgram, checkRepl)
import Test.Tasty.HUnit qualified as HUnit

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual = HUnit.assertEqual

assertRight :: (Show e) => String -> Either e a -> IO a
assertRight name (Left err) = HUnit.assertFailure $ name ++ " failed with: " ++ show err
assertRight _ (Right x) = return x

assertLeft :: String -> Either e a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = HUnit.assertFailure $ name ++ ": expected error"

assertOk :: (Show e) => String -> Either e a -> IO ()
assertOk _ (Right _) = return ()
assertOk name (Left err) = HUnit.assertFailure $ name ++ ": expected success but got " ++ show err

assertExpr :: String -> TypedExpr -> Env -> Value -> IO ()
assertExpr name expr env expected = do
  result <- runExceptT (runStateT (evalExpr expr) env)
  case result of
    Left err -> error $ name ++ " failed: " ++ show err
    Right (val, _) ->
      HUnit.assertEqual name expected val

assertEvalError :: String -> TypedExpr -> Env -> EvalError -> IO ()
assertEvalError name expr env expectedErr = do
  result <- runExceptT (runStateT (evalExpr expr) env)
  case result of
    Left err ->
      if err == expectedErr
        then return ()
        else HUnit.assertFailure $ name ++ ": expected error '" ++ show expectedErr ++ "' but got '" ++ show err ++ "'"
    Right (val, _) -> HUnit.assertFailure $ name ++ ": expected error but got " ++ show val

pos0 :: AlexPosn
pos0 = AlexPn 0 1 1

identFrom :: String -> Ident
identFrom = Ident pos0

emptyEnv :: Env
emptyEnv = initialValueEnv

lookupValue :: String -> Env -> Maybe Value
lookupValue _ [] = Nothing
lookupValue name (scope : rest) =
  case Map.lookup name scope of
    Just ref -> Just (unsafePerformIO (readIORef ref))
    Nothing -> lookupValue name rest

withTokens :: String -> String -> (ParsedRepl -> IO ()) -> IO ()
withTokens name source f = do
  ast <- assertRight ("parse " ++ name) $ runAlex source parseRepl
  f ast

checkOk :: String -> String -> IO ()
checkOk name source =
  withTokens name source $ \ast ->
    case checkRepl ast of
      Right _ -> return ()
      Left err -> error $ name ++ " failed: " ++ show err

checkErr :: String -> String -> IO ()
checkErr name source =
  withTokens name source $ \ast ->
    case checkRepl ast of
      Left _ -> return ()
      Right _ -> error $ name ++ ": expected type error but passed"

evalParsed :: String -> ParsedRepl -> IO (Either EvalError (Value, Env))
evalParsed name ast =
  case checkRepl ast of
    Left err -> error $ name ++ " type check failed: " ++ show err
    Right typedAst -> runExceptT (runStateT (evalRepl typedAst) emptyEnv)

evalProgramFromSource :: String -> IO (Either Error Value)
evalProgramFromSource source =
  case genAst source parseProgram of
    Left parseErr -> return $ Left $ ParseError parseErr
    Right ast -> case checkProgram ast of
      Left typeErr -> return $ Left $ TypeError typeErr
      Right typedAst -> do
        result <- runExceptT $ runStateT (evalProgram typedAst) initialValueEnv
        return $ case result of
          Left err -> Left $ EvalError err
          Right (val, _) -> Right val
