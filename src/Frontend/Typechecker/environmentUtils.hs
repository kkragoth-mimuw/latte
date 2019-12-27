module Frontend.Typechecker.EnvironmentUtils where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsLatte

import Frontend.Typechecker.Environment
import Frontend.Typechecker.TCM
import Frontend.Typechecker.Errors


extractVariableType :: Ident -> TCM Type
extractVariableType ident = do
    env <- ask

    case Map.lookup ident $ typesMap env of
        Just (type', _) -> return type'
        _ -> throwError $ initTypecheckError $ TCUndeclaredVariable ident


checkIfIsAlreadyDeclaredAtCurrentLevel :: Ident -> TCM ()
checkIfIsAlreadyDeclaredAtCurrentLevel ident = do
    env <- ask

    case Map.lookup ident $ typesMap env of
        Just (_, levelDeclared) | levelDeclared >= (level env) -> throwError $ initTypecheckError (TCRedeclaration ident)
        _ -> return ()

increaseLevel :: TCEnv -> TCEnv
increaseLevel env = env { level = (level env) + 1}