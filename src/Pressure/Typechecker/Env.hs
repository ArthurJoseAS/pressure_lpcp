module Pressure.Typechecker.Env where

import Control.Monad.State (StateT, get, gets, modify, put)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Pressure.Language.Types (Mutability, Type)
import Pressure.Typechecker.Error (Error)

type TypeEnv = [Map String (Type, Mutability)]

type LoopStack = [[Type]]

type CheckState = (TypeEnv, LoopStack)

type Check a = StateT CheckState (Either Error) a

getEnv :: Check TypeEnv
getEnv = gets fst

putEnv :: TypeEnv -> Check ()
putEnv env = modify (\(_, ls) -> (env, ls))

modifyEnv :: (TypeEnv -> TypeEnv) -> Check ()
modifyEnv f = modify (first f)

lookupName :: String -> TypeEnv -> Maybe (Type, Mutability)
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just tm -> Just tm
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> (Type, Mutability) -> TypeEnv -> TypeEnv
bindInCurrentScope name tm [] = [Map.singleton name tm]
bindInCurrentScope name tm (scope : rest) =
  Map.insert name tm scope : rest

pushScope :: TypeEnv -> TypeEnv
pushScope env = Map.empty : env

popScope :: TypeEnv -> TypeEnv
popScope [] = []
popScope (_ : rest) = rest

withScope :: Check a -> Check a
withScope action = do
  modifyEnv pushScope
  result <- action
  modifyEnv popScope
  return result

-- Loop context helpers

pushLoop :: Check ()
pushLoop = modify $ second ([] :)

popLoop :: Check [Type]
popLoop = do
  (env, ls) <- get
  case ls of
    (breaks : rest) -> do
      put (env, rest)
      return breaks
    [] -> error "internal: popLoop with empty loop stack"

recordBreak :: Maybe Type -> Check ()
recordBreak Nothing = return ()
recordBreak (Just t) =
  modify
    ( \(env, ls) ->
        case ls of
          (breaks : rest) -> (env, (t : breaks) : rest)
          [] -> (env, ls)
    )
