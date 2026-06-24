module Pressure.Language.Types where

import Data.List (intercalate)

data IntSize = I8 | I16 | I32 | I64
  deriving (Show, Eq)

data FloatSize = F32 | F64
  deriving (Show, Eq)

data Mutability = Mutable | Constant
  deriving (Show, Eq)

data Sign = Signed | Unsigned
  deriving (Show, Eq)

data Type
  = BoolT
  | IntT Sign IntSize
  | FloatT FloatSize
  | FnT [Type] Type
  | StringT
  | UnitT
  | TypeT
  deriving (Show, Eq)

prettyType :: Type -> String
prettyType = \case
  BoolT -> "bool"
  IntT Signed I8 -> "i8"
  IntT Signed I16 -> "i16"
  IntT Signed I32 -> "i32"
  IntT Signed I64 -> "i64"
  IntT Unsigned I8 -> "u8"
  IntT Unsigned I16 -> "u16"
  IntT Unsigned I32 -> "u32"
  IntT Unsigned I64 -> "u64"
  FloatT F32 -> "f32"
  FloatT F64 -> "f64"
  FnT params ret -> "fn(" ++ intercalate ", " (map prettyType params) ++ ") -> " ++ prettyType ret
  StringT -> "string"
  UnitT -> "unit"
  TypeT -> "type"

data UnaryOp
  = NegOp
  | NotOp
  | AmpersandOp
  deriving (Show, Eq)

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp = \case
  NegOp -> "-"
  NotOp -> "!"
  AmpersandOp -> "&"

data BinaryOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | AndOp
  | OrOp
  | EqOp
  | NeqOp
  | LtOp
  | LeqOp
  | GtOp
  | GeqOp
  deriving (Show, Eq)

prettyBinaryOp :: BinaryOp -> String
prettyBinaryOp = \case
  AddOp -> "+"
  SubOp -> "-"
  MulOp -> "*"
  DivOp -> "/"
  AndOp -> "and"
  OrOp -> "or"
  EqOp -> "=="
  NeqOp -> "!="
  LtOp -> "<"
  LeqOp -> "<="
  GtOp -> ">"
  GeqOp -> ">="
