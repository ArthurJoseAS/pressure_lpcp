module Pressure.Interpreter.Value where

import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Pressure.Language.Ast (TypedBlock, TypedParam)
import Pressure.Language.Types

type ValueEnv = [Map String (IORef Value)]

type LValuePath = [String]

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VString String
  | VUnit
  | VType Type
  | VEmpty
  | VFunction [TypedParam] Type TypedBlock ValueEnv
  | VArray [Value]
  | VBuiltin String
  | VStruct [(String, Value)]
  | VPtr Mutability (IORef Value) LValuePath
  deriving (Eq)

instance Show Value where
  show = \case
    VInt _ _ i -> show i
    VFloat _ f -> show f
    VBool True -> "true"
    VBool False -> "false"
    VString s -> show s
    VUnit -> "()"
    VType t -> prettyType t
    VFunction {} -> "<function>"
    VArray list -> show list
    VBuiltin n -> "<builtin " ++ n ++ ">"
    VStruct fields -> "struct { " ++ intercalate ", " (map (\(n, v) -> n ++ " = " ++ show v) fields) ++ " }"
    VPtr Mutable _ path -> "&mut " ++ intercalate "." path
    VPtr Constant _ path -> "&" ++ intercalate "." path
    VEmpty -> undefined
