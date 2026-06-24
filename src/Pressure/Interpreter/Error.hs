module Pressure.Interpreter.Error where

import Pressure.Interpreter.Value (Value)
import Pressure.Language.Lexer (AlexPosn, prettyPosn)

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
  | CastError AlexPosn String
  deriving (Eq, Show)

panic :: String -> m a
panic = error . ("panic: " ++)

panicAt :: AlexPosn -> String -> m a
panicAt pos msg = error $ prettyPosn pos ++ ": panic: " ++ msg

errorInfo :: Error -> (Maybe AlexPosn, String)
errorInfo = \case
  RuntimeError (DivisionByZero pos) -> (Just pos, "division by zero")
  RuntimeError (Overflow pos) -> (Just pos, "integer overflow")
  RuntimeError (Underflow pos) -> (Just pos, "integer underflow")
  RuntimeError (CastError pos msg) -> (Just pos, "cast error: " ++ msg)
  _ -> (Nothing, "internal error: unexpected control flow")
