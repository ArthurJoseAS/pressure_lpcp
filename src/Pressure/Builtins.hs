module Pressure.Builtins where

import Data.Map.Strict qualified as Map
import Pressure.Interpreter.Value (Value (..), ValueEnv)
import Pressure.Language.Types
import Pressure.Typechecker.Env (TypeEnv)

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
