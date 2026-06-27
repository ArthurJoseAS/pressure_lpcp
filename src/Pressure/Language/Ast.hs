module Pressure.Language.Ast where

import Data.List (intercalate)
import Pressure.Language.Lexer (AlexPosn (..))
import Pressure.Language.Types

newtype Program stmt = Program [TopLevel stmt]
  deriving (Show, Eq)

-- TODO: Better define this to only support struct fields.
newtype TopLevel stmt = TopLevelStmt stmt
  deriving (Show, Eq)

data Block stmt expr = Block [stmt] (Maybe expr)
  deriving (Show, Eq)

newtype Repl stmt expr = Repl [ReplInput stmt expr]
  deriving (Show, Eq)

data ReplInput stmt expr
  = ReplStmt stmt
  | ReplExpr expr
  deriving (Show, Eq)

type ParsedProgram = Program ParsedStmt

type ParsedTopLevel = TopLevel ParsedStmt

type ParsedBlock = Block ParsedStmt ParsedExpr

type ParsedReplInput = ReplInput ParsedStmt ParsedExpr

type ParsedRepl = Repl ParsedStmt ParsedExpr

type TypedProgram = Program TypedStmt

type TypedTopLevel = TopLevel TypedStmt

type TypedBlock = Block TypedStmt TypedExpr

type TypedReplInput = ReplInput TypedStmt TypedExpr

type TypedRepl = Repl TypedStmt TypedExpr

data ParsedStmt = ParsedStmt
  { parsedStmtPos :: AlexPosn,
    parsedStmtKind :: ParsedStmtKind
  }
  deriving (Show, Eq)

data ParsedStmtKind
  = ParsedDeclStmt ParsedDecl
  | ParsedExprStmt ParsedExpr
  | ParsedAssignStmt ParsedAssign
  deriving (Show, Eq)

data TypedStmt = TypedStmt
  { typedStmtPos :: AlexPosn,
    typedStmtKind :: TypedStmtKind
  }
  deriving (Show, Eq)

data TypedStmtKind
  = TypedDeclStmt TypedDecl
  | TypedExprStmt TypedExpr
  | TypedAssignStmt TypedAssign
  deriving (Show, Eq)

data ParsedDecl
  = ParsedValueDecl Mutability Ident (Maybe TypeSyntax) ParsedExpr
  deriving (Show, Eq)

data TypedDecl
  = TypedValueDecl Mutability Ident Type TypedExpr
  deriving (Show, Eq)

-- NOTE : Members cant be assigned
data ParsedLValue
  = ParsedLVar Ident
  | ParsedLAccess ParsedLValue Ident
  deriving (Show, Eq)

data ParsedAssign
  = ParsedAssign ParsedLValue ParsedExpr
  deriving (Show, Eq)

data TypedLValue
  = TypedLVar Ident Type
  | TypedLAccess TypedLValue Ident Type
  deriving (Show, Eq)

data TypedAssign
  = TypedAssign TypedLValue TypedExpr
  deriving (Show, Eq)

data Param = Param Ident TypeSyntax
  deriving (Show, Eq)

data TypedParam = TypedParam Ident Type
  deriving (Show, Eq)

data Ident = Ident AlexPosn String
  deriving (Show, Eq)

identPos :: Ident -> AlexPosn
identPos (Ident pos _) = pos

identName :: Ident -> String
identName (Ident _ name) = name

data TypeSyntax = TypeSyntax
  { typePos :: AlexPosn,
    typeKind :: TypeSyntaxKind
  }
  deriving (Show, Eq)

data TypeSyntaxKind
  = NameSyntax String
  | BoolSyntax
  | IntSyntax Sign IntSize
  | FloatSyntax FloatSize
  | FnSyntax [TypeSyntax] TypeSyntax
  | ArraySyntax TypeSyntax
  | StringSyntax
  | UnitSyntax
  | StructSyntax [StructItem]
  | TySyntax
  | AnyTypeSyntax
  deriving (Show, Eq)

data StructItem
  = StructField Ident TypeSyntax
  | StructMemberDecl ParsedDecl
  deriving (Show, Eq)

data ParsedExpr = ParsedExpr
  { parsedExprPos :: AlexPosn,
    parsedExprKind :: ParsedExprKind
  }
  deriving (Show, Eq)

data ParsedExprKind
  = ParsedIntLit Integer
  | ParsedFloatLit Double
  | ParsedBoolLit Bool
  | ParsedStringLit String
  | ParsedUnitLit
  | ParsedTypeLit TypeSyntax
  | ParsedBinaryExpr BinaryOp ParsedExpr ParsedExpr
  | ParsedUnaryExpr UnaryOp ParsedExpr
  | ParsedVarExpr Ident
  | ParsedIfExpr ParsedExpr ParsedBlock (Maybe ParsedBlock)
  | ParsedWhileExpr ParsedExpr ParsedBlock (Maybe ParsedBlock)
  | ParsedFnExpr [Param] TypeSyntax ParsedBlock
  | ParsedCallExpr ParsedExpr [ParsedExpr]
  | ParsedStructInit (Maybe Ident) [ParsedAssign]
  | ParsedMemberAccess ParsedExpr Ident
  | ParsedTypeExpr TypeSyntax
  | ParsedArrayLit [ParsedExpr]
  | ParsedIndexExpr ParsedExpr ParsedExpr
  | ParsedBreakExpr ParsedExpr
  | ParsedContinueExpr
  deriving (Show, Eq)

data TypedExpr = TypedExpr
  { typedExprPos :: AlexPosn,
    typedExprType :: Type,
    typedExprKind :: TypedExprKind
  }
  deriving (Show, Eq)

data TypedExprKind
  = TypedIntLit Integer
  | TypedFloatLit Double
  | TypedBoolLit Bool
  | TypedStringLit String
  | TypedUnitLit
  | TypedTypeLit Type
  | TypedBinaryExpr BinaryOp TypedExpr TypedExpr
  | TypedUnaryExpr UnaryOp TypedExpr
  | TypedVarExpr Ident
  | TypedIfExpr TypedExpr TypedBlock (Maybe TypedBlock)
  | TypedWhileExpr TypedExpr TypedBlock (Maybe TypedBlock)
  | TypedFnExpr [TypedParam] Type TypedBlock
  | TypedCallExpr TypedExpr [TypedExpr]
  | TypedStructInit (Maybe Ident) [(String, TypedExpr)]
  | TypedMemberAccess TypedExpr Ident
  | TypedArrayLit [TypedExpr]
  | TypedIndexExpr TypedExpr TypedExpr
  | TypedBreakExpr TypedExpr
  | TypedContinueExpr
  deriving (Show, Eq)
