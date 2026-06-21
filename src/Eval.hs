module Eval
  ( Error (..),
    Eval,
    Env,
    evalExpr,
    evalStmt,
    evalBlock,
    evalReplInput,
    evalProgram,
  )
where

import Ast.Syntax
  ( BinaryOp (..),
    Block (..),
    Decl (..),
    Expr (..),
    ExprKind (..),
    FloatSize (..),
    Ident (..),
    IntSize (..),
    Program (..),
    Repl (..),
    Sign (..),
    Stmt (..),
    StmtKind (..),
    TopLevel (..),
    Type (..),
    Value (..),
  )
import Ast.Typecheck (TypedBlock, TypedExpr, TypedProgram, TypedStmt, TypedTopLevel)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (StateT, get, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data Error
  = RuntimeError String
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

type Env = Map String Value

type Eval a = StateT Env (Except Error) a

evalExpr :: TypedExpr -> Eval Value
evalExpr (Expr _ kind) = case kind of
  IntLit i -> return (VInt Signed I32 i)
  FloatLit f -> return (VFloat F64 f)
  BoolLit b -> return (VBool b)
  BinaryExpr op l r -> evalBinaryOp op l r
  VarExpr (Ident _ i) -> evalVarExpr i
  IfExpr c t elseBlock -> evalIfExpr c t elseBlock

evalIfExpr :: TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalIfExpr c t me = do
  v <- evalExpr c
  case v of
    VBool True -> evalBlock t
    VBool False -> maybe (return VUnit) evalBlock me
    _ -> throwError $ RuntimeError "if condition must be bool"

evalStmt :: TypedStmt -> Eval Value
evalStmt (Stmt _ stmt) = case stmt of
  DeclStmt (ValueDecl _ (Ident _ i) mt me) -> evalDeclExpr i mt me
  ExprStmt expr -> evalExpr expr >> return VUnit

evalBlock :: TypedBlock -> Eval Value
evalBlock (Block stmts expr) = do
  mapM_ evalStmt stmts
  maybe (return VUnit) evalExpr expr

evalBinaryOp :: BinaryOp -> TypedExpr -> TypedExpr -> Eval Value
evalBinaryOp op l r = do
  vl <- evalExpr l
  vr <- evalExpr r
  case op of
    AddOp -> evalNumericBin (+) (+) vl vr
    SubOp -> evalNumericBin (-) (-) vl vr
    MulOp -> evalNumericBin (*) (*) vl vr
    DivOp -> evalDiv vl vr
    AndOp -> evalBoolBin (&&) vl vr
    OrOp -> evalBoolBin (||) vl vr
    EqOp -> evalEq vl vr
    NeqOp -> evalNeq vl vr
    LtOp -> evalNumericCmp (<) (<) vl vr
    LeqOp -> evalNumericCmp (<=) (<=) vl vr
    GtOp -> evalNumericCmp (>) (>) vl vr
    GeqOp -> evalNumericCmp (>=) (>=) vl vr

evalNumericBin :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Eval Value
evalNumericBin intOp floatOp va vb = case (asNumber va, asNumber vb) of
  (Just na, Just nb) -> case coerceNumericPair na nb of
    Left err -> throwError $ RuntimeError err
    Right (RuntimeInt s k a, RuntimeInt _ _ b) -> return (VInt s k (intOp a b))
    Right (RuntimeFloat k a, RuntimeFloat _ b) -> return (VFloat k (floatOp a b))
    Right _ -> throwError $ RuntimeError "internal error"
  _ -> throwError $ RuntimeError "invalid operands"

evalDiv :: Value -> Value -> Eval Value
evalDiv va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError "division by zero"
  VFloat _ 0 -> throwError $ RuntimeError "division by zero"
  _ -> evalNumericBin div (/) va vb

evalNumericCmp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp intCmp floatCmp va vb = case (asNumber va, asNumber vb) of
  (Just na, Just nb) -> case coerceNumericPair na nb of
    Left err -> throwError $ RuntimeError err
    Right (RuntimeInt _ _ a, RuntimeInt _ _ b) -> return (VBool (intCmp a b))
    Right (RuntimeFloat _ a, RuntimeFloat _ b) -> return (VBool (floatCmp a b))
    Right _ -> throwError $ RuntimeError "internal error"
  _ -> throwError $ RuntimeError "invalid operands"

evalEq :: Value -> Value -> Eval Value
evalEq (VBool a) (VBool b) = return (VBool (a == b))
evalEq va vb = case (asNumber va, asNumber vb) of
  (Just na, Just nb) -> case coerceNumericPair na nb of
    Left err -> throwError $ RuntimeError err
    Right (RuntimeInt _ _ a, RuntimeInt _ _ b) -> return (VBool (a == b))
    Right (RuntimeFloat _ a, RuntimeFloat _ b) -> return (VBool (a == b))
    Right _ -> throwError $ RuntimeError "internal error"
  _ -> throwError $ RuntimeError "invalid operands"

evalNeq :: Value -> Value -> Eval Value
evalNeq va vb = do
  v <- evalEq va vb
  case v of
    VBool b -> return (VBool (not b))
    _ -> throwError $ RuntimeError "internal error"

evalBoolBin :: (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
evalBoolBin op va vb = case (va, vb) of
  (VBool a, VBool b) -> return (VBool (op a b))
  _ -> throwError $ RuntimeError "invalid operands"

evalVarExpr :: String -> Eval Value
evalVarExpr n = do
  env <- get
  case Map.lookup n env of
    Just v -> return v
    Nothing -> throwError $ RuntimeError ("undefined variable: " ++ n)

evalDeclExpr :: String -> Maybe Type -> Maybe TypedExpr -> Eval Value
evalDeclExpr n mt me = do
  val <- case me of
    Just e -> evalExpr e
    Nothing -> case mt of
      Just t -> return (defaultValue t)
      Nothing -> throwError $ RuntimeError "declaration lacks both type and initializer"
  modify (Map.insert n val)
  return VUnit

evalReplInput :: Repl Type -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s -> evalStmt s >> return VUnit

evalProgram :: TypedProgram -> Eval Value
evalProgram (Program stmts) = mapM_ evalTopLevel stmts >> return VUnit

evalTopLevel :: TypedTopLevel -> Eval Value
evalTopLevel (TopLevelStmt stmt) = evalStmt stmt

defaultValue :: Type -> Value
defaultValue = \case
  IntType _ s k -> VInt s k 0
  FloatType _ k -> VFloat k 0
  BoolType _ -> VBool False
  UnitType -> VUnit
  TypeName _ -> VEmpty

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

coerceNumericPair :: RuntimeNumber -> RuntimeNumber -> Either String (RuntimeNumber, RuntimeNumber)
coerceNumericPair n m = case (n, m) of
  (RuntimeInt s1 k1 a, RuntimeInt s2 k2 b) ->
    if s1 == s2 && k1 == k2
      then Right (RuntimeInt s1 k1 a, RuntimeInt s2 k2 b)
      else Left "mismatched integer types"
  (RuntimeFloat k1 a, RuntimeFloat k2 b) ->
    if k1 == k2
      then Right (RuntimeFloat k1 a, RuntimeFloat k2 b)
      else Left "mismatched float types"
  (RuntimeInt _ _ a, RuntimeFloat _ b) -> Right (RuntimeFloat F64 (fromIntegral a), RuntimeFloat F64 b)
  (RuntimeFloat _ a, RuntimeInt _ _ b) -> Right (RuntimeFloat F64 a, RuntimeFloat F64 (fromIntegral b))
