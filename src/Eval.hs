module Eval
  ( Eval,
    Env,
    evalExpr,
    evalReplInput,
    evalProgram,
  )
where

import Ast
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Error = String

type Env = Map String Value

type Eval a = StateT Env (Either Error) a

evalExpr :: Expr -> Eval Value
evalExpr (IntLit _ i) = return (VInt i)
evalExpr (FloatLit _ f) = return (VFloat f)
evalExpr (BoolLit _ b) = return (VBool b)
evalExpr (VarExpr _ (Ident _ name)) = do
  env <- get
  case Map.lookup name env of
    Just v -> return v
    Nothing -> lift $ Left ("undefined variable: " ++ name)
evalExpr (BinaryExpr _ op l r) = evalBinaryOp op l r
evalExpr (DeclExpr _ (ValueDecl _ (Ident _ name) maybeType maybeExpr)) = do
  val <- case maybeExpr of
    Just e -> evalExpr e
    Nothing -> case maybeType of
      Just t -> return (defaultValue t)
      Nothing -> lift $ Left "declaration lacks both type and initializer"
  modify (Map.insert name val)
  return VUnit

defaultValue :: Type -> Value
defaultValue = \case
  IntType _ -> VInt 0
  FloatType _ -> VFloat 0
  BoolType _ -> VBool False
  UnitType -> VUnit
  TypeName _ -> VEmpty

evalBinaryOp :: BinaryOp -> Expr -> Expr -> Eval Value
evalBinaryOp op l r = do
  vl <- evalExpr l
  vr <- evalExpr r
  let mkInt n = return (VInt n)
      mkFloat n = return (VFloat n)
      divZero = lift $ Left "division by zero"
      invalid = lift $ Left $ "invalid operands for " ++ show op
  case (op, vl, vr) of
    (AddOp, VInt a, VInt b) -> mkInt (a + b)
    (AddOp, VFloat a, VFloat b) -> mkFloat (a + b)
    (AddOp, VInt a, VFloat b) -> mkFloat (fromIntegral a + b)
    (AddOp, VFloat a, VInt b) -> mkFloat (a + fromIntegral b)
    (SubOp, VInt a, VInt b) -> mkInt (a - b)
    (SubOp, VFloat a, VFloat b) -> mkFloat (a - b)
    (SubOp, VInt a, VFloat b) -> mkFloat (fromIntegral a - b)
    (SubOp, VFloat a, VInt b) -> mkFloat (a - fromIntegral b)
    (MulOp, VInt a, VInt b) -> mkInt (a * b)
    (MulOp, VFloat a, VFloat b) -> mkFloat (a * b)
    (MulOp, VInt a, VFloat b) -> mkFloat (fromIntegral a * b)
    (MulOp, VFloat a, VInt b) -> mkFloat (a * fromIntegral b)
    (DivOp, _, VInt 0) -> divZero
    (DivOp, _, VFloat 0) -> divZero
    (DivOp, VInt a, VInt b) -> mkInt (a `div` b)
    (DivOp, VInt a, VFloat b) -> mkFloat (fromIntegral a / b)
    (DivOp, VFloat a, VInt b) -> mkFloat (a / fromIntegral b)
    (DivOp, VFloat a, VFloat b) -> mkFloat (a / b)
    _ -> invalid

evalReplInput :: Repl -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt e -> evalExpr e >> return VUnit

evalProgram :: Program -> Eval Value
evalProgram (Program stmts) = mapM_ evalTopLevel stmts >> return VUnit

evalTopLevel :: TopLevel -> Eval Value
evalTopLevel (TopLevelStmt expr) = evalExpr expr
