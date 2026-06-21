module Type
  ( Error (..),
    typeOf,
    checkProgram,
    checkReplInput,
  )
where

import Ast
import Control.Monad (unless)
import Data.Functor (void)
import Lexer (AlexPosn)

data Error
  = TypeMismatch AlexPosn Type Type
  | UnsupportedOp AlexPosn BinaryOp Type Type
  | MissingAnnotation AlexPosn
  deriving (Show, Eq)

typeOf :: Expr -> Either Error Type
typeOf = \case
  IntLit pos _ -> Right (IntType pos Signed I32)
  FloatLit pos _ -> Right (FloatType pos F64)
  BoolLit pos _ -> Right (BoolType pos)
  BinaryExpr pos op l r -> do
    t1 <- typeOf l
    t2 <- typeOf r
    checkBinaryOp pos op t1 t2
  DeclExpr _ decl -> do
    checkDecl decl
    Right UnitType
  VarExpr pos _ -> Right (TypeName (Ident pos "_var"))

checkDecl :: Decl -> Either Error ()
checkDecl = \case
  ValueDecl _ ident Nothing Nothing -> Left (MissingAnnotation (identPos ident))
  ValueDecl _ _ Nothing (Just e) -> void (typeOf e)
  ValueDecl _ _ (Just _) Nothing -> Right ()
  ValueDecl _ ident (Just annot) (Just e) -> do
    t <- typeOf e
    unless (compatible annot t) $
      Left (TypeMismatch (identPos ident) annot t)

checkProgram :: Program -> Either Error ()
checkProgram (Program toplevels) = mapM_ checkTopLevel toplevels

checkReplInput :: Repl -> Either Error ()
checkReplInput = \case
  ReplStmt expr -> void (typeOf expr)
  ReplExpr expr -> void (typeOf expr)

checkTopLevel :: TopLevel -> Either Error ()
checkTopLevel (TopLevelStmt expr) = void (typeOf expr)

compatible :: Type -> Type -> Bool
compatible t1 t2 = case (t1, t2) of
  (IntType {}, IntType {}) -> True
  (FloatType {}, FloatType {}) -> True
  (FloatType {}, IntType {}) -> True
  (BoolType _, BoolType _) -> True
  (TypeName _, _) -> True
  (_, TypeName _) -> True
  (_, _) -> False

checkBinaryOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkBinaryOp pos op t1 t2 = case op of
  AddOp -> checkNumericOp pos op t1 t2
  SubOp -> checkNumericOp pos op t1 t2
  MulOp -> checkNumericOp pos op t1 t2
  DivOp -> checkNumericOp pos op t1 t2
  AndOp -> checkBoolOp pos op t1 t2
  OrOp -> checkBoolOp pos op t1 t2
  EqOp -> checkEqualityOp pos op t1 t2
  NeqOp -> checkEqualityOp pos op t1 t2
  LtOp -> checkOrderedOp pos op t1 t2
  LeqOp -> checkOrderedOp pos op t1 t2
  GtOp -> checkOrderedOp pos op t1 t2
  GeqOp -> checkOrderedOp pos op t1 t2

checkNumericOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkNumericOp pos op t1 t2
  | numericCompatible t1 t2 = Right (promoteNumeric pos t1 t2)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

checkBoolOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkBoolOp pos op t1 t2
  | isBoolLike t1 && isBoolLike t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

checkEqualityOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkEqualityOp pos op t1 t2
  | isBoolLike t1 && isBoolLike t2 = Right (BoolType pos)
  | numericCompatible t1 t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

checkOrderedOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkOrderedOp pos op t1 t2
  | numericCompatible t1 t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

numericCompatible :: Type -> Type -> Bool
numericCompatible t1 t2 = case (t1, t2) of
  (IntType _ s1 k1, IntType _ s2 k2) -> s1 == s2 && k1 == k2
  (FloatType _ k1, FloatType _ k2) -> k1 == k2
  (IntType {}, FloatType {}) -> True
  (FloatType {}, IntType {}) -> True
  (TypeName _, _) -> True
  (_, TypeName _) -> True
  _ -> False

isBoolLike :: Type -> Bool
isBoolLike = \case
  BoolType _ -> True
  TypeName _ -> True
  _ -> False

promoteNumeric :: AlexPosn -> Type -> Type -> Type
promoteNumeric pos t1 t2 = case (t1, t2) of
  (IntType _ s k, IntType {}) -> IntType pos s k
  (FloatType _ k, FloatType {}) -> FloatType pos k
  (IntType {}, FloatType {}) -> FloatType pos F64
  (FloatType {}, IntType {}) -> FloatType pos F64
  _ -> t1

identPos :: Ident -> AlexPosn
identPos (Ident pos _) = pos
