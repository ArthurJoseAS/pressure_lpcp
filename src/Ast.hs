module Ast
  ( Program (..),
    TopLevel (..),
    Repl (..),
    Decl (..),
    Ident (..),
    Mutability (..),
    Type (..),
    BinaryOp (..),
    Expr (..),
    Value (..),
  )
where

import Lexer (AlexPosn)

data Program = Program [TopLevel]
  deriving (Show, Eq)

data TopLevel = TopLevelStmt Expr
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

data Type
  = TypeName Ident
  | BoolType AlexPosn
  | IntType AlexPosn
  | FloatType AlexPosn
  | UnitType
  deriving (Show, Eq)

data BinaryOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
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
  = VInt Integer
  | VFloat Double
  | VBool Bool
  | VUnit
  | VEmpty
  deriving (Eq)

instance Show Value where
  show = \case
    VInt i -> show i
    VFloat f -> show f
    VBool b -> show b
    VUnit -> "()"
    VEmpty -> undefined
