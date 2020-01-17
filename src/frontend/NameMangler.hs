module NameMangler where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import Text.Printf

import AbsLatte

type NMM a = (ReaderT Env (StateT Store IO)) a

runNameMangler :: (NameMangleable program) => program -> IO Program
runNameMangler program = do
    (res, store) <- runStateT (runReaderT (nameMangle program) initEnv) initStore
    return res

class NameMangleable f where
    nameMangle :: f -> NMM Program

data Env = Env {
    level :: Integer,
    vars :: Map.Map Ident VariableInfo
}

initEnv = Env {
    level = 0,
    vars = Map.empty
}

increaseLevel :: Env -> Env
increaseLevel env = env { level = (level env) + 1}

data Store = Store {
    functionVars :: [VariableInfo]
}

initStore = Store {
    functionVars = []
}

data VariableInfo = VariableInfo {
    ident :: Ident,
    levelDeclared :: Integer,
    type' :: Type
}

nameMangleVariableInfo :: VariableInfo -> String
nameMangleVariableInfo variableInfo = printf("%s_%s_%s") (showIdent $ ident variableInfo) (show $ levelDeclared variableInfo) (showType $ type' variableInfo)

showIdent :: Ident -> String
showIdent (Ident s) = s

showType :: Type -> String
showType Int = "i"
showType Str = "str"
showType Boolean = "b"

instance NameMangleable Program where
    nameMangle (Program topdefs) = do
        nameMangledTopDefs <- mapM nameMangleTopDef topdefs
        return $ Program nameMangledTopDefs

nameMangleTopDef :: TopDef -> NMM TopDef
nameMangleTopDef (FnDef fnType ident args block@(Block stmts)) = do
    modify (\state -> state {
            functionVars = []
        })
    (_, nameMangledStmts) <- nameMangleStmts stmts
    functionVariables <- gets functionVars
    let functionVariablesDeclarations = map (\functionVar -> Decl (type' functionVar) [NoInit (Ident (nameMangleVariableInfo functionVar))]) functionVariables
    -- todo: functionVariables to decls
    return $ FnDef fnType ident args (Block (functionVariablesDeclarations ++ nameMangledStmts))

nameMangleStmts :: [Stmt] -> NMM (Env, [Stmt])
nameMangleStmts [] = do
    env <- ask
    return (env, [])
nameMangleStmts (x:xs) = do
    (envX, stmtsX) <- nameMangleStmt x
    (envXS, stmtsXS) <- local (const envX) (nameMangleStmts xs)
    return (envXS, stmtsX ++ stmtsXS)

nameMangleStmt :: Stmt -> NMM (Env, [Stmt])
nameMangleStmt (Decl type' []) = do
    env <- ask
    return (env, [])
nameMangleStmt (Decl type' (x:xs)) = do
    (envX, stmtX) <- nameMangleDeclItem type' x
    (envXS, stmtsXS) <- local (const envX) (nameMangleStmt (Decl type' xs))
    return (envXS, [stmtX] ++ stmtsXS)
newMangleStmt (BStmt (Block stmts)) = local increaseLevel (newMangleStmts stmts)
nameMangleStmt stmt = do
    env <- ask
    return (env, [stmt])

nameMangleDeclItem :: Type -> Item -> NMM (Env, Stmt)
nameMangleDeclItem type' (Init ident expr) = do
    env <- ask
    let varLevel = level env

    let variableInfo = VariableInfo {
        ident = ident,
        type' = type',
        levelDeclared = varLevel
    }
    
    modify (\store -> store {
        functionVars = (functionVars store) ++ [variableInfo]
    })

    let newEnv = env {
        vars = Map.insert ident variableInfo (vars env)
    }

    let stmt = Ass (LValue (nameMangleVariableInfo variableInfo)) expr

    return (newEnv, stmt)
