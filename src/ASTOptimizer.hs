module ASTOptimizer where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Bool

import AbsLatte

type OM a = (ReaderT Env (StateT Store IO)) a

runASTOptimizer :: (Optimizable program) => program -> IO Program
runASTOptimizer program = do
    (res, store) <- runStateT (runReaderT (optimize program) initEnv) initStore
    return res

class Optimizable f where
    optimize :: f -> OM Program

data Env = Env {

}

initEnv = Env {

}

data Store = Store {
    functions :: Map.Map Ident (Type, Bool)
}

initStore = Store {
    functions = Map.fromList (predefinedFunctions)
}

instance Optimizable Program where
    optimize program@(Program topdefs) = do
        optimizedTopDefs <- mapM optimizeTopDef topdefs
        return $ Program optimizedTopDefs

optimizeTopDef :: TopDef -> OM TopDef
optimizeTopDef topDef = do
    return topDef

predefinedFunctions :: [(Ident, (Type, Bool))]
predefinedFunctions = ([
        (Ident "printInt", ((Fun Void [Int]), True)),
        (Ident "printString", ((Fun Void [Str]), True)),
        (Ident "error", ((Fun Void []), True)),
        (Ident "readInt", ((Fun Int []), True)),
        (Ident "readString", ((Fun Str []), True)),
        (Ident "__concatStrings", ((Fun Str [Str, Str]), True))
    ])