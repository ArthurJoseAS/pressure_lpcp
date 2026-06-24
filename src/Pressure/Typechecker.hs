module Pressure.Typechecker
  ( Error (..),
    TypeEnv,
    checkRepl,
    checkReplWithEnv,
    checkReplInput,
    checkProgram,
    errorInfo,
  )
where

import Pressure.Typechecker.Check (checkProgram, checkRepl, checkReplInput, checkReplWithEnv)
import Pressure.Typechecker.Env (TypeEnv)
import Pressure.Typechecker.Error (Error (..), errorInfo)
