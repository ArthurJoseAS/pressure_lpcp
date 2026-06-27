module Pressure.Language.Parser.TestUtil
  ( parse,
    singleDecl,
    singleExpr,
    isIntSyntax,
    isIntLit,
    isBinary,
    expect,
  )
where

import Pressure.Language.Ast
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseRepl)
import Pressure.Language.Types (BinaryOp (..))
import Pressure.TestUtil (assertRight)

parse :: String -> String -> IO ParsedRepl
parse name source = assertRight name $ runAlex source parseRepl

singleDecl :: ParsedRepl -> Maybe ParsedDecl
singleDecl = \case
  Repl [ReplStmt (ParsedStmt _ (ParsedDeclStmt decl))] -> Just decl
  _ -> Nothing

singleExpr :: ParsedRepl -> Maybe ParsedExpr
singleExpr = \case
  Repl [ReplStmt (ParsedStmt _ (ParsedExprStmt expr))] -> Just expr
  Repl [ReplExpr expr] -> Just expr
  _ -> Nothing

isIntSyntax :: TypeSyntax -> Bool
isIntSyntax (TypeSyntax _ (IntSyntax _ _)) = True
isIntSyntax _ = False

isIntLit :: Integer -> ParsedExpr -> Bool
isIntLit expected = \case
  ParsedExpr _ (ParsedIntLit actual) -> expected == actual
  _ -> False

isBinary :: BinaryOp -> (ParsedExpr -> Bool) -> (ParsedExpr -> Bool) -> ParsedExpr -> Bool
isBinary expectedOp left right = \case
  ParsedExpr _ (ParsedBinaryExpr actualOp l r) -> expectedOp == actualOp && left l && right r
  _ -> False

expect :: String -> Bool -> ParsedRepl -> IO ()
expect _ True _ = return ()
expect name False ast = error $ "unexpected AST for " ++ name ++ ": " ++ show ast
