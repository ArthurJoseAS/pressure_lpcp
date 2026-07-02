module Pressure.Interpreter.Env where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, modify)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Pressure.Interpreter.Error
import Pressure.Interpreter.Value
import Pressure.Language.Lexer (AlexPosn)
import Pressure.Language.Types
import System.IO.Unsafe (unsafePerformIO)

type Env = [Map String (IORef Value)]

type Eval a = StateT Env (ExceptT EvalError IO) a

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double
  deriving (Show, Eq)

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

defaultValue :: Type -> Value
defaultValue = \case
  IntT s k -> VInt s k 0
  FloatT k -> VFloat k 0
  BoolT -> VBool False
  FnT _ _ -> VEmpty
  UnitT -> VUnit
  PtrT _ _ -> VEmpty
  -- Recursively calls defaultValue for each field. Ignores constants
  StructT fields _ -> VStruct (map (\(name, fieldType) -> (name, defaultValue fieldType)) fields)
  _ -> VEmpty

withNumbers :: AlexPosn -> (RuntimeNumber -> RuntimeNumber -> m Value) -> Value -> Value -> m Value
withNumbers pos f va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> f na nb
    _ -> panicAt pos "invalid operands reached evaluator"

-- | Read the IORef for a name in the env chain.
readName :: String -> Env -> Eval (Maybe Value)
readName _ [] = return Nothing
readName name (scope : rest) =
  case Map.lookup name scope of
    Just ref -> do
      v <- liftIO (readIORef ref)
      return (Just v)
    Nothing -> readName name rest

-- | Find the IORef for a name in the env chain (no read).
findRef :: String -> Env -> Maybe (IORef Value)
findRef _ [] = Nothing
findRef name (scope : rest) =
  case Map.lookup name scope of
    Just ref -> Just ref
    Nothing -> findRef name rest

-- | Create a new binding (action form, used in `modify`-free code paths).
bindInCurrentScope :: String -> Value -> Env -> Env
bindInCurrentScope name val = \case
  [] -> unsafePerformIO $ do
    ref <- newIORef val
    pure $ [Map.singleton name ref]
  (scope : rest) -> unsafePerformIO $ do
    ref <- newIORef val
    pure $ Map.insert name ref scope : rest

-- bindInCurrentScope name val env = unsafePerformIO $ do
--   ref <- newIORef val
--   return (bindRefInCurrentScope name ref env)

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

updateInScope :: String -> Value -> Env -> Env
updateInScope name val env =
  case findRef name env of
    Just ref -> System.IO.Unsafe.unsafePerformIO (writeIORef ref val) `seq` env
    Nothing -> env

-- | Read a value starting at an IORef, walking a path through struct fields.
readPath :: IORef Value -> LValuePath -> Eval Value
readPath ref path = do
  v <- liftIO (readIORef ref)
  return (walkRead v path)
  where
    walkRead val [] = val
    walkRead (VStruct fields) (fname : more) =
      case lookup fname fields of
        Just v -> walkRead v more
        Nothing -> error "field not found in struct value"
    walkRead _ _ = error "invalid path for read"

-- | Write a value at a path starting from an IORef. The whole struct chain
-- up to the root is rebuilt and the IORef is updated.
writePath :: IORef Value -> LValuePath -> Value -> Eval ()
writePath ref path newVal = do
  curVal <- liftIO (readIORef ref)
  let updated = walkUpdate curVal path
  liftIO (writeIORef ref updated)
  where
    walkUpdate _ [] = newVal
    walkUpdate (VStruct fields) (fname : more) =
      VStruct (map updateField fields)
      where
        updateField (n, v)
          | n == fname = (n, walkUpdate v more)
          | otherwise = (n, v)
    walkUpdate _ _ = error "invalid path for write"
