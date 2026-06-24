module Pressure.Builtins where

import Control.Monad (unless, zipWithM_)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Pressure.Interpreter.Env (Eval)
import Pressure.Interpreter.Error (panicAt)
import Pressure.Interpreter.Value (Value (..), ValueEnv)
import Pressure.Language.Ast (Ident (..), TypedExpr (..), TypedExprKind (..))
import Pressure.Language.Lexer (AlexPosn)
import Pressure.Language.Types
import Pressure.Typechecker.Env (Check, TypeEnv)
import Pressure.Typechecker.Error (Error (..))

initialValueEnv :: ValueEnv
initialValueEnv =
  [ Map.fromList
      [ ("@read", VBuiltin "@read"),
        ("@printf", VBuiltin "@printf")
      ]
  ]

initialTypeEnv :: TypeEnv
initialTypeEnv =
  [ Map.fromList
      [ ("@read", (FnT [] StringT, Constant)),
        ("@printf", (FnT [StringT] UnitT, Constant))
      ]
  ]

dispatchBuiltin :: AlexPosn -> String -> [Value] -> Eval Value
dispatchBuiltin pos name args = case name of
  "@read" -> dispatchRead pos args
  "@printf" -> dispatchPrintf pos args
  _ -> panicAt pos ("unknown builtin: " ++ name)

dispatchRead :: AlexPosn -> [Value] -> Eval Value
dispatchRead pos = \case
  [] -> VString <$> liftIO getLine
  _ -> panicAt pos "@read takes no arguments"

dispatchPrintf :: AlexPosn -> [Value] -> Eval Value
dispatchPrintf pos = \case
  VString fmt : args -> do
    let placeholders = countPlaceholders fmt
    if placeholders /= length args
      then panicAt pos ("@printf: expected " ++ show placeholders ++ " arguments for placeholders, got " ++ show (length args))
      else do
        let rendered = renderFormat fmt args
        liftIO $ putStr rendered
        return VUnit
  _ -> panicAt pos "@printf requires a string format as first argument"

checkPrintfCall :: AlexPosn -> TypedExpr -> [TypedExpr] -> Check TypedExpr
checkPrintfCall pos callee args = case args of
  [] -> liftEither $ Left $ InvalidPrintf pos "expected at least a format string argument"
  (fmtExpr : formatArgs) -> do
    unless (typedExprType fmtExpr == StringT) $ liftEither $ Left $ InvalidPrintf pos "first argument must be a string"
    case typedExprKind fmtExpr of
      TypedStringLit fmt -> do
        let placeholders = countPlaceholders fmt
        unless (placeholders == length formatArgs) $ liftEither $ Left $ InvalidPrintf pos $ placeholdersErr placeholders
        zipWithM_ checkPrintableFormatArg [1 ..] formatArgs
      _ -> liftEither $ Left $ InvalidPrintf pos "format string must be a literal"
    return $ TypedExpr pos UnitT (TypedCallExpr callee args)
    where
      placeholdersErr placeholders = "expected " ++ show placeholders ++ " arguments for placeholders, got " ++ show (length formatArgs)

checkPrintableFormatArg :: Int -> TypedExpr -> Check ()
checkPrintableFormatArg idx arg =
  unless (isPrintable (typedExprType arg)) $
    liftEither $
      Left $
        InvalidPrintf (typedExprPos arg) $
          "argument " ++ show idx ++ " has non-printable type '" ++ prettyType (typedExprType arg) ++ "'"

isPrintable :: Type -> Bool
isPrintable = \case
  IntT {} -> True
  FloatT {} -> True
  BoolT -> True
  StringT -> True
  UnitT -> True
  _ -> False

isPrintf :: TypedExpr -> Bool
isPrintf (TypedExpr _ _ (TypedVarExpr (Ident _ "@printf"))) = True
isPrintf _ = False

countPlaceholders :: String -> Int
countPlaceholders = go 0
  where
    go n [] = n
    go n ('{' : '}' : rest) = go (n + 1) rest
    go n (_ : rest) = go n rest

renderFormat :: String -> [Value] -> String
renderFormat fmt args = go fmt args ""
  where
    go [] _ acc = acc
    go ('{' : '}' : rest) (v : vs) acc = go rest vs (acc ++ formatValue v)
    go (c : rest) args' acc = go rest args' (acc ++ [c])

formatValue :: Value -> String
formatValue (VString s) = s
formatValue v = show v
