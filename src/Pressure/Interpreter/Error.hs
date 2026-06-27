module Pressure.Interpreter.Error where

import Pressure.Interpreter.Value (Value)
import Pressure.Language.Lexer (AlexPosn (AlexPn), prettyPosn)
import Pressure.Language.Parser (parseErrorInfo)
import Pressure.Typechecker.Error qualified as Type

data EvalError
  = RuntimeError RuntimeError
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

errorInfo :: EvalError -> (Maybe AlexPosn, String)
errorInfo = \case
  RuntimeError (DivisionByZero pos) -> (Just pos, "division by zero")
  RuntimeError (Overflow pos) -> (Just pos, "integer overflow")
  RuntimeError (Underflow pos) -> (Just pos, "integer underflow")
  RuntimeError (CastError pos msg) -> (Just pos, "cast error: " ++ msg)
  _ -> (Nothing, "internal error: unexpected control flow")

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

data Error
  = ParseError String
  | TypeError Type.Error
  | EvalError EvalError
  | Exit
  deriving (Eq, Show)

render :: String -> Error -> String
render source err =
  let (mPos, msg) = case err of
        ParseError e -> parseErrorInfo e
        TypeError e -> Type.errorInfo e
        EvalError e -> errorInfo e
        Exit -> (Nothing, "")
      header = case mPos of
        Just pos -> prettyPosn pos ++ ": " ++ msg
        Nothing -> msg
      snippet = maybe "" (sourceSnippet source) mPos
   in if null header then "" else header ++ "\n" ++ snippet

sourceSnippet :: String -> AlexPosn -> String
sourceSnippet source (AlexPn _ line col) =
  let srcLines = lines source
      targetLine = if line > 0 && line <= length srcLines then srcLines !! (line - 1) else ""
      caret = replicate (max 0 (col - 1)) ' ' ++ "^"
      indent = "  "
   in indent ++ targetLine ++ "\n" ++ indent ++ caret
