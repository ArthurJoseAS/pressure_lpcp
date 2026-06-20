module Lang (repl, run) where

import Ast (Repl (..), Value)
import Control.Monad (forever, when)
import Control.Monad.Except (ExceptT, catchError, liftEither, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, lift, liftIO, mapStateT)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.Map.Strict qualified as Map
import Eval (Env, Eval, evalReplInput)
import Parser (genAst, parseRepl)
import System.IO (hFlush, isEOF, stdout)
import Type qualified

-- TODO: Separate REPL into separate module and add support for control characters.

type REPL a = StateT Env (ExceptT Error IO) a

repl :: IO ()
repl = do
  _ <- runExceptT $ evalStateT replLoop Map.empty
  putStrLn "Goodbye!"

replLoop :: REPL ()
replLoop = forever $ replStep `catchError` handleError
  where
    handleError Exit = lift $ throwError Exit
    handleError err = liftIO $ putStrLn $ render err

run :: String -> IO ()
run input = do
  _ <- runExceptT $ evalStateT (eval input) Map.empty
  return ()

replStep :: REPL ()
replStep = do
  liftIO $ putStr ">> " >> hFlush stdout

  done <- liftIO isEOF
  when done $ lift $ throwError Exit

  line <- liftIO getLine
  when (trim line == ":q") $ lift $ throwError Exit

  (val, ast) <- eval line

  liftIO $ case ast of
    ReplExpr _ -> print val
    ReplStmt _ -> return ()

eval :: String -> REPL (Value, Ast.Repl)
eval input = do
  ast <- liftEither $ first ParseError $ genAst input parseRepl
  _ <- liftEither $ first TypeError $ Type.checkReplInput ast
  val <- liftEval $ evalReplInput ast

  return (val, ast)

data Error
  = ParseError String
  | TypeError Type.Error
  | RuntimeError String
  | Exit
  deriving (Show, Eq)

liftEval :: Eval Value -> REPL Value
liftEval = mapStateT $ liftEither . (first RuntimeError)

render :: Error -> String
render = \case
  ParseError e -> "parser error: " ++ e
  TypeError e -> "type error: " ++ show e
  RuntimeError e -> "runtime error: " ++ e
  Exit -> ""

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
