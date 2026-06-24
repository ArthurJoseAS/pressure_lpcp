module Pressure.Interpreter.Env where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Pressure.Interpreter.Error
import Pressure.Interpreter.Value
import Pressure.Language.Lexer (AlexPosn)
import Pressure.Language.Types

type Env = [Map String Value]

type Eval a = StateT Env (ExceptT Error IO) a

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double
  deriving (Show, Eq)

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

withNumbers :: AlexPosn -> (RuntimeNumber -> RuntimeNumber -> m Value) -> Value -> Value -> m Value
withNumbers pos f va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> f na nb
    _ -> panicAt pos "invalid operands reached evaluator"

lookupName :: String -> ValueEnv -> Maybe Value
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> Value -> ValueEnv -> ValueEnv
bindInCurrentScope name val [] = [Map.singleton name val]
bindInCurrentScope name val (scope : rest) =
  Map.insert name val scope : rest

updateInScope :: String -> Value -> ValueEnv -> ValueEnv
updateInScope _ _ [] = []
updateInScope name val (scope : rest)
  | Map.member name scope = Map.insert name val scope : rest
  | otherwise = scope : updateInScope name val rest

pushScope :: ValueEnv -> ValueEnv
pushScope env = Map.empty : env

popScope :: ValueEnv -> ValueEnv
popScope [] = []
popScope (_ : rest) = rest

withScope :: Eval a -> Eval a
withScope action = do
  modify pushScope
  result <- action
  modify popScope
  return result
