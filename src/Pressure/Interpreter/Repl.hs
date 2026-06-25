module Pressure.Interpreter.Repl (repl, run) where

import Control.Monad (forever, void, when)
import Control.Monad.Except (ExceptT (..), catchError, liftEither, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, get, lift, liftIO, put, runStateT)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Pressure.Builtins (initialTypeEnv, initialValueEnv)
import Pressure.Interpreter.Env (Eval)
import Pressure.Interpreter.Eval qualified as Eval
import Pressure.Interpreter.Value (Value (..), ValueEnv)
import Pressure.Language.Ast (ParsedRepl)
import Pressure.Language.Lexer (AlexPosn (..), prettyPosn)
import Pressure.Language.Parser (genAst, parseErrorInfo, parseRepl)
import Pressure.Typechecker.Check qualified as Type
import Pressure.Typechecker.Env qualified as Type
import Pressure.Typechecker.Error qualified as Type
import System.IO (hFlush, isEOF, stdout)

type ReplState = (ValueEnv, Type.TypeEnv)

type REPL a = StateT ReplState (ExceptT Error IO) a

repl :: IO ()
repl = do
  _ <- runExceptT $ evalStateT replLoop (initialValueEnv, initialTypeEnv)
  putStrLn "Goodbye!"

replLoop :: REPL ()
replLoop = forever $ replStep `catchError` handleReplExit
  where
    handleReplExit :: Error -> REPL ()
    handleReplExit Exit = lift $ throwError Exit
    handleReplExit _ = return ()

replStep :: REPL ()
replStep = do
  liftIO $ putStr ">> " >> hFlush stdout
  done <- liftIO isEOF
  when done $ lift $ throwError Exit
  line <- liftIO getLine
  when (trim line == ":q") $ lift $ throwError Exit
  ( do
      (val, _) <- eval line
      liftIO $ case val of
        VUnit -> return ()
        _ -> print val
    )
    `catchError` \err -> case err of
      Exit -> lift $ throwError Exit
      _ -> handleError line err

run :: String -> IO ()
run input = do
  _ <- runExceptT $ evalStateT (run' `catchError` \err -> handleError input err) (initialValueEnv, initialTypeEnv)
  return ()
  where
    run' = void $ eval input

handleError :: String -> Error -> REPL ()
handleError source err = liftIO $ putStrLn $ render source err

eval :: String -> REPL (Value, ParsedRepl)
eval input = do
  ast <- liftEither $ first ParseError $ genAst input parseRepl
  (_, typeEnv) <- get
  (typedAst, nextTypeEnv) <- liftEither $ first TypeError $ Type.checkReplWithEnv typeEnv ast
  val <- liftEval nextTypeEnv $ Eval.evalRepl typedAst
  return (val, ast)

data Error
  = ParseError String
  | TypeError Type.Error
  | RuntimeError Eval.Error
  | Exit
  deriving (Show, Eq)

liftEval :: Type.TypeEnv -> Eval Value -> REPL Value
liftEval nextTypeEnv action = do
  (env, _) <- get
  result <- liftIO $ runExceptT (runStateT action env)
  case result of
    Left err -> lift $ throwError $ RuntimeError err
    Right (val, nextEnv) -> do
      put (nextEnv, nextTypeEnv)
      return val

render :: String -> Error -> String
render source err =
  let (mPos, msg) = case err of
        ParseError e -> parseErrorInfo e
        TypeError e -> let (pos, m) = Type.errorInfo e in (Just pos, m)
        RuntimeError e -> Eval.errorInfo e
        Exit -> (Nothing, "")
      header = case mPos of
        Just pos -> prettyPosn pos ++ ": " ++ msg
        Nothing -> msg
      snippet = maybe "" (sourceSnippet source) mPos
   in if null header then "" else header ++ "\n" ++ snippet

sourceSnippet :: String -> AlexPosn -> String
sourceSnippet source (AlexPn _ line col) =
  let srcLines = lines source
      targetLine = if line > 0 && line <= length srcLines then srcLines !! (line - 1) else ""
      caret = replicate (max 0 (col - 1)) ' ' ++ "^"
      indent = "  "
   in indent ++ targetLine ++ "\n" ++ indent ++ caret

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
