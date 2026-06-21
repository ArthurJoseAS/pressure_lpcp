module Ast
  ( Program (..),
    TopLevel (..),
    Repl (..),
    Decl (..),
    Ident (..),
    Mutability (..),
    Sign (..),
    IntSize (..),
    FloatSize (..),
    Type (..),
    BinaryOp (..),
    Expr (..),
    Value (..),
  )
where

import Lexer (AlexPosn)

newtype Program = Program [TopLevel]
  deriving (Show, Eq)

newtype TopLevel = TopLevelStmt Expr
  deriving (Show, Eq)

data Repl
  = ReplStmt Expr
  | ReplExpr Expr
  deriving (Show, Eq)

data Decl
  = ValueDecl Mutability Ident (Maybe Type) (Maybe Expr)
  deriving (Show, Eq)

data Ident = Ident AlexPosn String
  deriving (Show, Eq)

data Mutability = Mutable | Constant
  deriving (Show, Eq)

data Sign = Signed | Unsigned
  deriving (Show, Eq)

data IntSize = I8 | I16 | I32 | I64
  deriving (Show, Eq)

data FloatSize = F32 | F64
  deriving (Show, Eq)

data Type
  = TypeName Ident
  | BoolType AlexPosn
  | IntType AlexPosn Sign IntSize
  | FloatType AlexPosn FloatSize
  | UnitType
  deriving (Show, Eq)

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

data Expr
  = IntLit AlexPosn Integer
  | FloatLit AlexPosn Double
  | BoolLit AlexPosn Bool
  | BinaryExpr AlexPosn BinaryOp Expr Expr
  | DeclExpr AlexPosn Decl
  | VarExpr AlexPosn Ident
  deriving (Show, Eq)

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VUnit
  | VEmpty
  deriving (Eq)

instance Show Value where
  show = \case
    VInt _ _ i -> show i
    VFloat _ f -> show f
    VBool True -> "true"
    VBool False -> "false"
    VUnit -> "()"
    VEmpty -> undefined
