module Pressure.Interpreter.Program where

import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.State (MonadIO (liftIO), runStateT)
import Data.Bifunctor (Bifunctor (first))
import Pressure.Builtins (initialValueEnv)
import Pressure.Interpreter.Error (Error (..), render)
import Pressure.Interpreter.Eval (evalProgram)
import Pressure.Language.Parser (genAst, parseProgram)
import Pressure.Typechecker.Check (checkProgram)

run :: FilePath -> IO ()
run file = do
  program <- readFile file
  result <- runExceptT $ do
    ast <- liftEither $ first ParseError $ genAst program parseProgram
    typedAst <- liftEither $ first TypeError $ checkProgram ast
    evalResult <- liftIO $ runExceptT $ runStateT (evalProgram typedAst) initialValueEnv
    case evalResult of
      Left err -> throwError $ EvalError err
      Right (val, _) -> return val
  case result of
    Left err -> putStrLn $ render program err
    Right _ -> pure ()
