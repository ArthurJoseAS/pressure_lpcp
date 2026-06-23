module Pressure.Interpreter.Value where

import Data.Map.Strict (Map)
import Pressure.Language.Ast (TypedBlock, TypedParam)
import Pressure.Language.Types

type ValueEnv = [Map String Value]

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VString String
  | VUnit
  | VEmpty
  | VFunction [TypedParam] Type TypedBlock ValueEnv
  | VBuiltin String
  deriving (Eq)

instance Show Value where
  show = \case
    VInt _ _ i -> show i
    VFloat _ f -> show f
    VBool True -> "true"
    VBool False -> "false"
    VString s -> show s
    VUnit -> "()"
    VFunction {} -> "<function>"
    VBuiltin n -> "<builtin " ++ n ++ ">"
    VEmpty -> undefined
