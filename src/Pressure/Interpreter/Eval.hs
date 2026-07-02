module Pressure.Interpreter.Eval
  ( RuntimeError (..),
    Eval,
    evalExpr,
    evalStmt,
    evalBlock,
    evalRepl,
    evalReplInput,
    evalProgram,
    errorInfo,
  )
where

import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.State (get, modify)
import Data.Fixed (mod')
import Data.IORef
import Data.Maybe (mapMaybe)
import Pressure.Builtins (dispatchBuiltin)
import Pressure.Interpreter.Env
import Pressure.Interpreter.Error
import Pressure.Interpreter.Value
import Pressure.Language.Ast
import Pressure.Language.Lexer (AlexPosn (..))
import Pressure.Language.Types

-- Expressions

evalExpr :: TypedExpr -> Eval Value
evalExpr (TypedExpr pos _ kind) = case kind of
  TypedIntLit i -> return (VInt Signed I32 i)
  TypedFloatLit f -> return (VFloat F64 f)
  TypedBoolLit b -> return (VBool b)
  TypedUnitLit -> return VUnit
  TypedTypeLit t -> return (VType t)
  TypedStringLit s -> return (VString s)
  TypedUnaryExpr op e -> evalUnaryExpr pos op e
  TypedAddrOfExpr isMut e -> evalAddrOfExpr pos isMut e
  TypedDerefExpr e -> evalDerefExpr pos e
  TypedBinaryExpr op l r -> evalBinaryExpr pos op l r
  TypedVarExpr (Ident ipos name) -> evalVarExpr ipos name
  TypedIfExpr c t elseBlock -> evalIfExpr pos c t elseBlock
  TypedWhileExpr c body mElse -> evalWhileExpr pos c body mElse
  TypedFnExpr params ret body -> VFunction params ret body <$> get
  TypedCallExpr callee args -> evalCallExpr pos callee args
  TypedStructInit _ fields -> do
    evaluatedFields <-
      mapM
        ( \(name, expr) -> do
            val <- evalExpr expr -- evaluate field expression (<id> = <expr>)
            return (name, val)
        )
        fields
    return $ VStruct evaluatedFields
  TypedMemberAccess expr fieldId -> do
    v <- evalExpr expr
    case v of
      VStruct fields ->
        -- fetches the field from the struct
        case lookup (identName fieldId) fields of
          Just val -> return val -- value found
          Nothing -> panicAt pos "field not found in struct value"
      _ -> panicAt pos "attempted to access member of non-struct value" -- value found
  TypedBreakExpr mExpr -> evalBreakExpr pos mExpr
  TypedContinueExpr -> evalContinueExpr pos
  TypedArrayLit exprs -> evalArrayLit exprs
  TypedIndexExpr base index -> evalIndexExpr pos base index

evalIfExpr :: AlexPosn -> TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalIfExpr pos c t mElse = do
  v <- evalExpr c
  case v of
    VBool True -> withScope (evalBlock t)
    VBool False -> maybe (return VUnit) (withScope . evalBlock) mElse
    _ -> panicAt pos "if condition must be bool reached evaluator"

evalWhileExpr :: AlexPosn -> TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalWhileExpr pos cond body mElse = loop
  where
    loop = do
      v <- evalExpr cond
      case v of
        VBool True -> do
          result <- tryLoopIteration (withScope (evalBlock body))
          maybe loop return result
        VBool False ->
          maybe (return VUnit) (withScope . evalBlock) mElse
        _ -> panicAt pos "while condition must be bool reached evaluator"

tryLoopIteration :: Eval Value -> Eval (Maybe Value)
tryLoopIteration action =
  catchError
    (action >> return Nothing)
    ( \case
        BreakSignal val -> return (Just val)
        ContinueSignal -> return Nothing
        other -> throwError other
    )

evalBreakExpr :: AlexPosn -> TypedExpr -> Eval Value
evalBreakExpr _ e = evalExpr e >>= \val -> throwError (BreakSignal val)

evalContinueExpr :: AlexPosn -> Eval Value
evalContinueExpr _ = throwError ContinueSignal

evalVarExpr :: AlexPosn -> String -> Eval Value
evalVarExpr pos name = do
  env <- get
  mval <- readName name env
  case mval of
    Just v -> return v
    Nothing -> panicAt pos ("undefined variable '" ++ name ++ "' reached evaluator")

evalCallExpr :: AlexPosn -> TypedExpr -> [TypedExpr] -> Eval Value
evalCallExpr pos callee args = do
  fn <- evalExpr callee
  argVals <- mapM evalExpr args
  callValue pos fn argVals

callValue :: AlexPosn -> Value -> [Value] -> Eval Value
callValue pos v args = case v of
  VBuiltin name -> dispatchBuiltin pos name args
  VFunction params _ body env -> do
    if length params /= length args
      then panicAt pos ("wrong number of arguments: expected " ++ show (length params) ++ ", got " ++ show (length args))
      else do
        callerEnv <- get
        modify $ const $ bindArgs params args (pushScope env)
        val <- evalBlock body
        modify $ const callerEnv
        return val
  _ -> panicAt pos "attempted to call non-function reached evaluator"

bindArgs :: [TypedParam] -> [Value] -> ValueEnv -> ValueEnv
bindArgs params argVals env = foldl bind env (zip params argVals)
  where
    bind e (TypedParam (Ident _ name) _, val) = bindInCurrentScope name val e

evalUnaryExpr :: AlexPosn -> UnaryOp -> TypedExpr -> Eval Value
evalUnaryExpr pos op e = do
  ve <- evalExpr e
  case op of
    NegOp -> evalNumericUn negate negate ve
    NotOp -> evalBooleanUn not ve
    AmpersandOp -> panicAt pos "not implemented: unary '&'"

evalAddrOfExpr :: AlexPosn -> Bool -> TypedExpr -> Eval Value
evalAddrOfExpr pos isMut e = do
  env <- get
  case typedExprLValueRefPath e env of
    Just (ref, path) -> pure (VPtr (if isMut then Mutable else Constant) ref path)
    Nothing -> panicAt pos "address-of requires an l-value"

evalDerefExpr :: AlexPosn -> TypedExpr -> Eval Value
evalDerefExpr pos e = do
  v <- evalExpr e
  case v of
    VPtr _ ref path -> readPath ref path
    _ -> panicAt pos "dereference of non-pointer value"

-- | Decompose a typed expression that should be an l-value into the root
-- IORef and the path that selects the value within it. The path is empty
-- for a plain variable and is extended with the field name for member access.
-- Dereferences return the underlying pointer's (ref, path).
typedExprLValueRefPath :: TypedExpr -> Env -> Maybe (IORef Value, LValuePath)
typedExprLValueRefPath e env = case e of
  TypedExpr _ _ (TypedVarExpr (Ident _ name)) -> fmap (,[]) (findRef name env)
  TypedExpr _ _ (TypedMemberAccess base (Ident _ name)) -> do
    (ref, path) <- typedExprLValueRefPath base env
    return (ref, path ++ [name])
  _ -> Nothing

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
    ModOp -> evalMod pos vl vr
    AndOp -> evalBoolBin pos (&&) vl vr
    OrOp -> evalBoolBin pos (||) vl vr
    EqOp -> evalEq pos vl vr
    NeqOp -> evalNeq pos vl vr
    LtOp -> evalNumericCmp pos (<) (<) vl vr
    LeqOp -> evalNumericCmp pos (<=) (<=) vl vr
    GtOp -> evalNumericCmp pos (>) (>) vl vr
    GeqOp -> evalNumericCmp pos (>=) (>=) vl vr

evalNumericBin :: AlexPosn -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Eval Value
evalNumericBin pos intOp floatOp = withNumbers pos go
  where
    go (RuntimeInt s k a) (RuntimeInt _ _ b) = return (VInt s k (intOp a b))
    go (RuntimeFloat k a) (RuntimeFloat _ b) = return (VFloat k (floatOp a b))
    go _ _ = panic "type mismatch in numeric bin"

evalDiv :: AlexPosn -> Value -> Value -> Eval Value
evalDiv pos va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  VFloat _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  _ -> evalNumericBin pos div (/) va vb

evalMod :: AlexPosn -> Value -> Value -> Eval Value
evalMod pos va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  VFloat _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  _ -> evalNumericBin pos mod mod' va vb

evalNumericCmp :: AlexPosn -> (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp pos intCmp floatCmp = withNumbers pos go
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
  TypedStmt _ (TypedDeclStmt (TypedValueDecl _ (Ident _ name) _ expr)) -> go bindInCurrentScope name expr
  TypedStmt _ (TypedAssignStmt (TypedAssign lhs expr)) -> do
    newVal <- evalExpr expr
    evalLValueUpdate lhs newVal
    return VUnit
  TypedStmt _ (TypedExprStmt expr) -> evalExpr expr >> return VUnit
  where
    go bind name expr = do
      val <- evalExpr expr
      modify (bind name val)
      return VUnit

evalLValueUpdate :: TypedExpr -> Value -> Eval ()
evalLValueUpdate lhs newVal = case typedExprKind lhs of
  TypedVarExpr (Ident _ name) ->
    modify (updateInScope name newVal)
  TypedMemberAccess base (Ident pos name) -> do
    baseVal <- evalLValue base
    case baseVal of
      VStruct fields -> do
        let newFields =
              map (\(n, v) -> if n == name then (n, newVal) else (n, v)) fields
        evalLValueUpdate base (VStruct newFields)
      _ -> panicAt pos "attempted to assign to field of non-struct value"
  TypedIndexExpr base idx -> do
    baseVal <- evalLValue base
    idxVal <- evalExpr idx
    -- TODO: This should only support usizes in the future.
    case (baseVal, idxVal) of
      (VArray vals, VInt _ _ i) -> do
        let hi = fromIntegral i
        if hi < 0 || hi >= length vals
          then panicAt (typedExprPos lhs) "array index out of bounds"
          else do
            let newVals = take hi vals ++ [newVal] ++ drop (hi + 1) vals
            evalLValueUpdate base (VArray newVals)
      _ -> panicAt (typedExprPos lhs) "attempted to index non-array value"
  TypedDerefExpr base -> do
    baseVal <- evalExpr base
    case baseVal of
      VPtr Mutable ref path -> writePath ref path newVal
      VPtr Constant _ _ -> panicAt (typedExprPos lhs) "assignment through *T is forbidden"
      _ -> panicAt (typedExprPos lhs) "invalid l-value in deref"
  _ -> panicAt (typedExprPos lhs) "invalid assignment target in evaluator"

evalLValue :: TypedExpr -> Eval Value
evalLValue expr = case typedExprKind expr of
  TypedVarExpr (Ident pos name) -> evalVarExpr pos name
  TypedMemberAccess base (Ident pos name) -> do
    v <- evalLValue base
    case v of
      VStruct fields ->
        case lookup name fields of
          Just val -> return val
          Nothing -> panicAt pos "field not found in struct value"
      _ -> panicAt pos "attempted to access member of non-struct value"
  TypedIndexExpr base idx -> do
    v <- evalLValue base
    idxVal <- evalExpr idx
    case (v, idxVal) of
      (VArray vals, VInt _ _ i) -> do
        let hi = fromIntegral i
        if hi < 0 || hi >= length vals
          then panicAt (typedExprPos expr) "array index out of bounds"
          else return (vals !! hi)
      _ -> panicAt (typedExprPos expr) "attempted to index non-array value"
  _ -> evalExpr expr

-- Function items

installFunctionItems :: [TypedStmt] -> Eval ()
installFunctionItems stmts = do
  let fns = mapMaybe functionItem stmts
  env <- get
  let extendedEnv = foldl addFn env fns
        where
          addFn env' (_, name, params, ret, body) =
            let closure = VFunction params ret body extendedEnv
             in bindInCurrentScope name closure env'
  modify (const extendedEnv)

functionItem :: TypedStmt -> Maybe (AlexPosn, String, [TypedParam], Type, TypedBlock)
functionItem = \case
  TypedStmt pos (TypedDeclStmt (TypedValueDecl Constant (Ident _ name) _ (TypedExpr _ _ (TypedFnExpr params ret body)))) -> Just (pos, name, params, ret, body)
  _ -> Nothing

-- TODO: Grrrr, remove this duplication
functionStmt :: TypedStmt -> Maybe TypedStmt
functionStmt s = case s of
  TypedStmt _ (TypedDeclStmt (TypedValueDecl Constant (Ident _ _) _ (TypedExpr _ _ (TypedFnExpr {})))) -> Just s
  _ -> Nothing

-- Blocks

evalBlock :: TypedBlock -> Eval Value
evalBlock (Block stmts expr) = do
  installFunctionItems stmts
  mapM_ evalStmt stmts
  maybe (return VUnit) evalExpr expr

-- Arrays

evalArrayLit :: [TypedExpr] -> Eval Value
evalArrayLit list = do
  exprList <- mapM evalExpr list
  return (VArray exprList)

evalIndexExpr :: AlexPosn -> TypedExpr -> TypedExpr -> Eval Value
evalIndexExpr pos base index = do
  baseExpr <- evalExpr base
  indexExpr <- evalExpr index
  haskellIndex <- case indexExpr of
    VInt _ _ i -> return (fromIntegral i)
    _ -> panicAt pos "indexing array with non integer value"
  case baseExpr of
    VArray vals ->
      if haskellIndex < 0 || haskellIndex >= length vals
        then panicAt pos ("array index out of bounds: index " ++ show haskellIndex ++ " on length " ++ show (length vals))
        else return (vals !! haskellIndex)
    _ -> panicAt pos "indxexing non array value"

-- Programs

evalProgram :: TypedProgram -> Eval Value
evalProgram (Program toplevels) = do
  modify pushScope
  let stmts = map topLevelStmt toplevels
  installFunctionItems stmts
  mapM_ evalStmt stmts
  env <- get
  mMain <- readName "main" env
  _ <- case mMain of
    Nothing -> panic "missing main in evaluator"
    Just f -> callValue (AlexPn 0 0 0) f [] -- FIXME: Use the correct position of main.
  return VUnit
  where
    topLevelStmt (TopLevelStmt stmt) = stmt

-- REPL

evalRepl :: TypedRepl -> Eval Value
evalRepl (Repl inputs) = do
  modify pushScope
  installFunctionItems $ mapMaybe isStmtAndFunctionItem inputs
  evaluated <- mapM evalReplInput inputs
  case reverse evaluated of
    [] -> return VUnit
    (x : _) -> return x
  where
    isStmtAndFunctionItem (ReplStmt s) = functionStmt s
    isStmtAndFunctionItem _ = Nothing

evalReplInput :: TypedReplInput -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s -> evalStmt s >> return VUnit
