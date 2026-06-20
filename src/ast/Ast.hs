module Ast
  ( Program (..),
    TopLevel (..),
    ReplInput (..),
    Decl (..),
    Ident (..),
    Mutability (..),
    Type (..),
    BinaryOp (..),
    Expr (..),
  )
where

import Lexer (AlexPosn)

data Program = Program [TopLevel]
  deriving (Show, Eq)

data TopLevel = TopLevelDecl Decl
  deriving (Show, Eq)

data ReplInput
  = ReplDecl Decl
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
  deriving (Show, Eq)

data BinaryOp
  = AddOp
  | MulOp
  | DivOp
  deriving (Show, Eq)

data Expr
  = IntLit AlexPosn Integer
  | FloatLit AlexPosn Double
  | BoolLit AlexPosn Bool
  | BinaryExpr AlexPosn BinaryOp Expr Expr
  deriving (Show, Eq)

data Value
  = VInt Integer
  | VFloat Double
  | VBool Bool
  deriving (Show, Eq)

type Error = String

evalExpr :: Expr -> Either Error Value
evalExpr (IntLit _ i) = Right (VInt i)
evalExpr (FloatLit _ f) = Right (VFloat f)
evalExpr (BoolLit _ b) = Right (BoolLit b)
