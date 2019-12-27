module Frontend.Typechecker.TCM where

import Control.Monad.Except
import Control.Monad.Reader

import Frontend.Typechecker.Errors
import Frontend.Typechecker.Environment

type TCM a = (ExceptT TypecheckErrorWithLogging (Reader TCEnv)) a