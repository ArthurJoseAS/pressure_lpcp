module Eval
  ( Error (..),
    RuntimeError (..),
    Eval,
    Env,
    evalExpr,
    evalStmt,
    evalBlock,
    evalReplInput,
    evalProgram,
    errorInfo,
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
    Mutability (..),
    Param (..),
    Program (..),
    Repl (..),
    Sign (..),
    Stmt (..),
    StmtKind (..),
    TopLevel (..),
    Type (..),
    UnaryOp (..),
    Value (..),
    typePosn,
  )
import Ast.Typecheck (TypedBlock, TypedExpr, TypedProgram, TypedStmt)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (StateT, get, lift, modify, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Lexer (AlexPosn (..), prettyPosn)

-- Types

data Error
  = RuntimeError RuntimeError
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

data RuntimeError
  = DivisionByZero AlexPosn
  | Overflow AlexPosn
  | Underflow AlexPosn
  deriving (Eq, Show)

panic :: String -> Eval a
panic = error . ("panic: " ++)

panicAt :: AlexPosn -> String -> Eval a
panicAt pos msg = error $ prettyPosn pos ++ ": panic: " ++ msg

errorInfo :: Error -> (Maybe AlexPosn, String)
errorInfo = \case
  RuntimeError (DivisionByZero pos) -> (Just pos, "division by zero")
  RuntimeError (Overflow pos) -> (Just pos, "integer overflow")
  RuntimeError (Underflow pos) -> (Just pos, "integer underflow")
  _ -> (Nothing, "internal error: unexpected control flow")

type Env = [Map String Value]

type Eval a = StateT Env (Except Error) a

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double

-- Environment helpers

lookupName :: String -> Env -> Maybe Value
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> Value -> Env -> Env
bindInCurrentScope name val [] = [Map.singleton name val]
bindInCurrentScope name val (scope : rest) =
  Map.insert name val scope : rest

globalEnv :: Env -> Env
globalEnv [] = []
globalEnv [scope] = [scope]
globalEnv (_ : rest) = globalEnv rest

pushScope :: Env -> Env
pushScope env = Map.empty : env

popScope :: Env -> Env
popScope [] = []
popScope (_ : rest) = rest

withScope :: Eval a -> Eval a
withScope action = do
  modify pushScope
  result <- action
  modify popScope
  return result

-- Values

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

defaultValue :: Type -> Value
defaultValue = \case
  IntType _ s k -> VInt s k 0
  FloatType _ k -> VFloat k 0
  BoolType _ -> VBool False
  FnType _ _ _ -> VEmpty
  UnitType -> VUnit
  TypeName _ -> VEmpty

withNumbers :: AlexPosn -> (RuntimeNumber -> RuntimeNumber -> Eval Value) -> Value -> Value -> Eval Value
withNumbers pos f va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> f na nb
    _ -> panicAt pos "invalid operands reached evaluator"

-- Expressions

evalExpr :: TypedExpr -> Eval Value
evalExpr (Expr typ kind) = case kind of
  IntLit i -> return (VInt Signed I32 i)
  FloatLit f -> return (VFloat F64 f)
  BoolLit b -> return (VBool b)
  UnaryExpr op e -> evalUnaryExpr (typePosn typ) op e
  BinaryExpr op l r -> evalBinaryExpr (typePosn typ) op l r
  VarExpr (Ident pos i) -> evalVarExpr pos i
  IfExpr c t elseBlock -> evalIfExpr (typePosn typ) c t elseBlock
  FnExpr params ret body -> evalFnExpr params ret body
  CallExpr callee args -> evalCallExpr (typePosn typ) callee args

evalIfExpr :: AlexPosn -> TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalIfExpr pos c t mElse = do
  v <- evalExpr c
  case v of
    VBool True -> withScope (evalBlock t)
    VBool False -> maybe (return VUnit) (withScope . evalBlock) mElse
    _ -> panicAt pos "if condition must be bool reached evaluator"

evalVarExpr :: AlexPosn -> String -> Eval Value
evalVarExpr pos name = do
  env <- get
  case lookupName name env of
    Just v -> return v
    Nothing -> panicAt pos ("undefined variable '" ++ name ++ "' reached evaluator")

evalFnExpr :: [Param] -> Type -> TypedBlock -> Eval Value
evalFnExpr params ret body =
  return $ VFunction params ret body

evalCallExpr :: AlexPosn -> TypedExpr -> [TypedExpr] -> Eval Value
evalCallExpr pos callee args = do
  fn <- evalExpr callee
  argVals <- mapM evalExpr args
  callValue pos fn argVals

callValue :: AlexPosn -> Value -> [Value] -> Eval Value
callValue pos (VFunction params _ body) argVals = do
  if length params /= length argVals
    then panicAt pos ("wrong number of arguments: expected " ++ show (length params) ++ ", got " ++ show (length argVals))
    else do
      callerEnv <- get
      modify $ const $ bindArgs params argVals (pushScope (globalEnv callerEnv))
      val <- evalBlock body
      modify $ const callerEnv
      return val
callValue pos _ _ = panicAt pos "attempted to call non-function reached evaluator"

bindArgs :: [Param] -> [Value] -> Env -> Env
bindArgs params argVals env = foldl bind env (zip params argVals)
  where
    bind e (Param (Ident _ name) _, val) = bindInCurrentScope name val e

evalUnaryExpr :: AlexPosn -> UnaryOp -> TypedExpr -> Eval Value
evalUnaryExpr pos op e = do
  ve <- evalExpr e
  case op of
    NegOp -> evalNumericUn negate negate ve
    NotOp -> evalBooleanUn not ve
    AmpersandOp -> panicAt pos "not implemented: unary '&'"

evalNumericUn :: (Integer -> Integer) -> (Double -> Double) -> Value -> Eval Value
evalNumericUn intOp floatOp v =
  case asNumber v of
    Just (RuntimeInt s k i) -> return (VInt s k (intOp i))
    Just (RuntimeFloat k d) -> return (VFloat k (floatOp d))
    Nothing -> panic "expected number"

evalBooleanUn :: (Bool -> Bool) -> Value -> Eval Value
evalBooleanUn op = \case
  VBool b -> return (VBool $ op b)
  _ -> panic "expected bool"

evalBinaryExpr :: AlexPosn -> BinaryOp -> TypedExpr -> TypedExpr -> Eval Value
evalBinaryExpr pos op l r = do
  vl <- evalExpr l
  vr <- evalExpr r
  case op of
    AddOp -> evalNumericBin pos (+) (+) vl vr
    SubOp -> evalNumericBin pos (-) (-) vl vr
    MulOp -> evalNumericBin pos (*) (*) vl vr
    DivOp -> evalDiv pos vl vr
    AndOp -> evalBoolBin pos (&&) vl vr
    OrOp -> evalBoolBin pos (||) vl vr
    EqOp -> evalEq pos vl vr
    NeqOp -> evalNeq pos vl vr
    LtOp -> evalNumericCmp pos (<) (<) vl vr
    LeqOp -> evalNumericCmp pos (<=) (<=) vl vr
    GtOp -> evalNumericCmp pos (>) (>) vl vr
    GeqOp -> evalNumericCmp pos (>=) (>=) vl vr

evalNumericBin :: AlexPosn -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Eval Value
evalNumericBin pos intOp floatOp va vb = withNumbers pos go va vb
  where
    go (RuntimeInt s k a) (RuntimeInt _ _ b) = return (VInt s k (intOp a b))
    go (RuntimeFloat k a) (RuntimeFloat _ b) = return (VFloat k (floatOp a b))
    go _ _ = panic "type mismatch in numeric bin"

evalDiv :: AlexPosn -> Value -> Value -> Eval Value
evalDiv pos va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  VFloat _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  _ -> evalNumericBin pos div (/) va vb

evalNumericCmp :: AlexPosn -> (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp pos intCmp floatCmp va vb = withNumbers pos go va vb
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (intCmp a b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (floatCmp a b))
    go _ _ = panic "type mismatch in numeric cmp"

evalEq :: AlexPosn -> Value -> Value -> Eval Value
evalEq _ (VBool a) (VBool b) = return (VBool (a == b))
evalEq pos va vb = withNumbers pos go va vb
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (a == b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (a == b))
    go _ _ = panic "type mismatch in eq"

evalNeq :: AlexPosn -> Value -> Value -> Eval Value
evalNeq pos va vb = do
  v <- evalEq pos va vb
  case v of
    VBool b -> return (VBool (not b))
    _ -> panic "type mismatch in neq"

evalBoolBin :: AlexPosn -> (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
evalBoolBin _ op va vb = case (va, vb) of
  (VBool a, VBool b) -> return (VBool (op a b))
  _ -> panic "invalid operands reached evaluator"

-- Statements

evalStmt :: TypedStmt -> Eval Value
evalStmt = \case
  s | isFunctionItemStmt s -> return VUnit
  Stmt _ (DeclStmt (ValueDecl _ (Ident pos name) mType mExpr)) -> evalDeclExpr pos name mType mExpr
  Stmt _ (ExprStmt expr) -> evalExpr expr >> return VUnit

evalDeclExpr :: AlexPosn -> String -> Maybe Type -> Maybe TypedExpr -> Eval Value
evalDeclExpr pos name mType mExpr = do
  val <- case mExpr of
    Just e -> evalExpr e
    Nothing -> case mType of
      Just t -> return (defaultValue t)
      Nothing -> panicAt pos "declaration lacks both type and initializer reached evaluator"
  modify (bindInCurrentScope name val)
  return VUnit

-- Function items

isFunctionItemStmt :: TypedStmt -> Bool
isFunctionItemStmt (Stmt _ (DeclStmt (ValueDecl Constant _ _ (Just (Expr _ (FnExpr {})))))) = True
isFunctionItemStmt _ = False

installFunctionItems :: [TypedStmt] -> Eval ()
installFunctionItems stmts = do
  let fns = mapMaybe functionItem stmts
  env <- get
  let extendedEnv = foldl addFn env fns
        where
          addFn env' (name, params, ret, body) =
            let closure = VFunction params ret body
             in bindInCurrentScope name closure env'
  put extendedEnv

functionItem :: TypedStmt -> Maybe (String, [Param], Type, TypedBlock)
functionItem = \case
  (Stmt _ (DeclStmt (ValueDecl Constant (Ident _ name) _ (Just (Expr _ (FnExpr params ret body)))))) -> Just (name, params, ret, body)
  _ -> Nothing

-- Blocks

evalBlock :: TypedBlock -> Eval Value
evalBlock (Block stmts expr) = do
  installFunctionItems stmts
  mapM_ evalStmt stmts
  maybe (return VUnit) evalExpr expr

-- Programs

evalProgram :: TypedProgram -> Eval Value
evalProgram (Program toplevels) = do
  modify pushScope
  let stmts = map topLevelStmt toplevels
  installFunctionItems stmts
  mapM_ evalStmt stmts
  return VUnit
  where
    topLevelStmt (TopLevelStmt stmt) = stmt

-- REPL

evalReplInput :: Repl Type -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s | isFunctionItemStmt s -> installFunctionItems [s] >> return VUnit
  ReplStmt s -> evalStmt s >> return VUnit
