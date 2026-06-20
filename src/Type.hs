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
  IntLit pos _ -> Right (IntType pos)
  FloatLit pos _ -> Right (FloatType pos)
  BoolLit pos _ -> Right (BoolType pos)
  BinaryExpr pos op l r -> do
    t1 <- typeOf l
    t2 <- typeOf r
    checkNumericOp pos op t1 t2
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
  (IntType _, IntType _) -> True
  (FloatType _, FloatType _) -> True
  (FloatType _, IntType _) -> True
  (BoolType _, BoolType _) -> True
  (TypeName _, _) -> True
  (_, TypeName _) -> True
  (_, _) -> False

checkNumericOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkNumericOp pos op t1 t2
  | not (isNumeric t1 && isNumeric t2) = Left (UnsupportedOp pos op t1 t2)
  | otherwise = Right (promoteNumeric pos t1 t2)

isNumeric :: Type -> Bool
isNumeric = \case
  IntType _ -> True
  FloatType _ -> True
  TypeName _ -> True
  _ -> False

promoteNumeric :: AlexPosn -> Type -> Type -> Type
promoteNumeric pos t1 t2 = case (t1, t2) of
  (FloatType _, _) -> FloatType pos
  (_, FloatType _) -> FloatType pos
  (_, _) -> IntType pos

identPos :: Ident -> AlexPosn
identPos (Ident pos _) = pos
