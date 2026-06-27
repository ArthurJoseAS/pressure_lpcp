module Pressure.Typechecker.Error where

import Pressure.Language.Lexer
import Pressure.Language.Types

data Error
  = TypeMismatch AlexPosn Type Type
  | UnsupportedOp AlexPosn BinaryOp Type Type
  | UnsupportedUnaryOp AlexPosn UnaryOp Type
  | DuplicateParams AlexPosn String
  | DuplicateFunction AlexPosn String
  | DuplicateDeclaration AlexPosn String
  | UndefinedVariable AlexPosn String
  | UndefinedType AlexPosn String
  | MutableType AlexPosn Type
  | NotCallable AlexPosn Type
  | ArityMismatch AlexPosn Int Int
  | AssignToConstant AlexPosn String
  | MissingLoopElse AlexPosn
  | ElseWithoutBreak AlexPosn
  | BreakOutsideLoop AlexPosn
  | ContinueOutsideLoop AlexPosn
  | NonUnitLoopBody AlexPosn Type
  | InvalidPrintf AlexPosn String
  | InvalidCast AlexPosn Type Type
  | MissingMain
  | InvalidMain AlexPosn Type
  deriving (Show, Eq)

errorPos :: Error -> Maybe AlexPosn
errorPos = \case
  TypeMismatch pos _ _ -> Just pos
  UnsupportedOp pos _ _ _ -> Just pos
  UnsupportedUnaryOp pos _ _ -> Just pos
  DuplicateParams pos _ -> Just pos
  DuplicateFunction pos _ -> Just pos
  DuplicateDeclaration pos _ -> Just pos
  UndefinedVariable pos _ -> Just pos
  UndefinedType pos _ -> Just pos
  MutableType pos _ -> Just pos
  NotCallable pos _ -> Just pos
  ArityMismatch pos _ _ -> Just pos
  AssignToConstant pos _ -> Just pos
  MissingLoopElse pos -> Just pos
  ElseWithoutBreak pos -> Just pos
  BreakOutsideLoop pos -> Just pos
  ContinueOutsideLoop pos -> Just pos
  NonUnitLoopBody pos _ -> Just pos
  InvalidPrintf pos _ -> Just pos
  InvalidCast pos _ _ -> Just pos
  MissingMain -> Nothing
  InvalidMain pos _ -> Just pos

errorInfo :: Error -> (Maybe AlexPosn, String)
errorInfo err =
  ( errorPos err,
    case err of
      MissingMain -> "missing main function in the top level declarations"
      TypeMismatch _ expected actual -> "type mismatch: expected '" ++ prettyType expected ++ "', found '" ++ prettyType actual ++ "'"
      UnsupportedOp _ op t1 t2 -> "cannot use operator '" ++ prettyBinaryOp op ++ "' on type '" ++ prettyType t1 ++ "' and '" ++ prettyType t2 ++ "'"
      UnsupportedUnaryOp _ op t -> "cannot use unary operator '" ++ prettyUnaryOp op ++ "' on type '" ++ prettyType t ++ "'"
      DuplicateParams _ name -> "duplicate parameter '" ++ name ++ "'"
      DuplicateFunction _ name -> "duplicate function '" ++ name ++ "'"
      DuplicateDeclaration _ name -> "duplicate declaration '" ++ name ++ "'"
      UndefinedVariable _ name -> "undefined variable '" ++ name ++ "'"
      UndefinedType _ name -> "undefined type '" ++ name ++ "'"
      MutableType _ t -> "cannot declare mutable value of '" ++ prettyType t ++ "'"
      NotCallable _ t -> "cannot call value of type '" ++ prettyType t ++ "'"
      ArityMismatch _ expected actual -> "wrong number of arguments: expected " ++ show expected ++ ", got " ++ show actual
      AssignToConstant _ name -> "cannot assign to constant '" ++ name ++ "'"
      MissingLoopElse _ -> "loop with break value must have else clause"
      ElseWithoutBreak _ -> "loop with else must have a break clause"
      BreakOutsideLoop _ -> "'break' outside of loop"
      ContinueOutsideLoop _ -> "'continue' outside of loop"
      NonUnitLoopBody _ t -> "loop body must have type '()', found '" ++ prettyType t ++ "'"
      InvalidPrintf _ msg -> "@printf: " ++ msg
      InvalidCast _ target actual -> "invalid cast to '" ++ prettyType target ++ "' from '" ++ prettyType actual ++ "'"
      InvalidMain _ t -> "expected main function with type '" ++ prettyType (FnT [] UnitT) ++ "' found '" ++ prettyType t ++ "'"
  )
