module NameMangler where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe (fromJust)
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

nameMangleTopDef (ClassDef ident classPoles) = do
    classPoles' <- mapM nameMangleClassPole classPoles
    return (ClassDef ident classPoles')
nameMangleTopDef (ClassDefExt ident baseIdent classPoles) = do
    classPoles' <- mapM nameMangleClassPole classPoles
    return (ClassDefExt ident baseIdent classPoles')

nameMangleClassPole :: ClassPole -> NMM ClassPole
-- nameMangleClassPole (ClassMethodDef type' ident args block) = do
nameMangleClassPole c = return c

nameMangleStmts :: [Stmt] -> NMM (Env, [Stmt])
nameMangleStmts [] = do
    env <- ask
    return (env, [])
nameMangleStmts (x:xs) = do
    (envX, stmtsX) <- nameMangleStmt x
    (envXS, stmtsXS) <- local (const envX) (nameMangleStmts xs)
    return (envXS, stmtsX ++ stmtsXS)

nameMangleStmt :: Stmt -> NMM (Env, [Stmt])
nameMangleStmt (Empty) = do
    env <- ask
    return (env, [])
nameMangleStmt (Decl type' []) = do
    env <- ask
    return (env, [])
nameMangleStmt (BStmt (Block stmts)) = local increaseLevel (nameMangleStmts stmts)
nameMangleStmt (Decl type' (x:xs)) = do
    (envX, stmtX) <- nameMangleDeclItem type' x
    (envXS, stmtsXS) <- local (const envX) (nameMangleStmt (Decl type' xs))
    return (envXS, [stmtX] ++ stmtsXS)
nameMangleStmt (Ass lvalue expr) = do
    env <- ask
    lvalue' <- nameMangleLValue lvalue
    expr' <- nameMangleExpr expr
    return (env, [Ass lvalue' expr'])
nameMangleStmt (Incr lvalue) = do
    env <- ask
    lvalue' <- nameMangleLValue lvalue
    return (env, [Incr lvalue'])
nameMangleStmt (Decr lvalue) = do
    env <- ask
    lvalue' <- nameMangleLValue lvalue
    return (env, [Decr lvalue'])
nameMangleStmt (Ret expr) = do
    env <- ask
    expr' <- nameMangleExpr expr
    return (env, [Ret expr'])
nameMangleStmt (VRet) = do
    env <- ask
    return (env, [VRet])
nameMangleStmt (Cond expr stmt) = do
    env <- ask
    expr' <- nameMangleExpr expr
    (_, stmts) <- nameMangleStmt stmt
    case stmts of
        [stmt'] -> return (env, [Cond expr' stmt'])
        _ -> return (env, [Cond expr' (BStmt (Block stmts))])
nameMangleStmt (CondElse expr stmt1 stmt2) = do
    env <- ask
    expr' <- nameMangleExpr expr
    (_, stmts1) <- nameMangleStmt stmt1
    (_, stmts2) <- nameMangleStmt stmt2
    let stmts1' = case stmts1 of
                    [stmt1'] -> stmt1'
                    _ -> (BStmt (Block stmts1))
    let stmts2' = case stmts2 of
                    [stmt2'] -> stmt2'
                    _ -> (BStmt (Block stmts2))
    return (env, [CondElse expr' stmts1' stmts2'])
nameMangleStmt (While expr stmt) = do
    env <- ask
    expr' <- nameMangleExpr expr
    (_, stmts) <- nameMangleStmt stmt
    let stmt' = case stmts of
                    [stmt'] -> stmt'
                    _ -> (BStmt (Block stmts))
    return (env, [While expr' stmt'])
nameMangleStmt (SExp expr) = do
    env <- ask
    expr' <- nameMangleExpr expr
    return (env, [SExp expr'])

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

    nameMangledExpr <- nameMangleExpr expr

    let stmt = Ass (LValue (Ident (nameMangleVariableInfo variableInfo))) nameMangledExpr

    return (newEnv, stmt)

-- nameMangleLValue :: LValue -> NMM LValue
-- nameMangleLValue (LValue ident) = do
--     varsMap <- asks vars0


nameMangleLValue :: LValue -> NMM LValue
nameMangleLValue (LValue ident) = do
    varsMap <- asks vars
    case Map.lookup ident varsMap of
        Nothing -> return (LValue ident) -- probably this.
        Just varInfo -> return (LValue (Ident (nameMangleVariableInfo varInfo)))

nameMangleLValue (LValueClassField lvalue ident) = do
    lvalue' <- nameMangleLValue lvalue
    return (LValueClassField lvalue' ident)
nameMangleLValue lvalue = error "todo"

nameMangleExpr :: Expr -> NMM (Expr)
nameMangleExpr (ELValue lvalue) = do
    lvalue' <- nameMangleLValue lvalue
    return (ELValue lvalue')
nameMangleExpr (EApp lvalue exprs) = do
    lvalue' <- nameMangleLValue lvalue
    exprs' <- mapM nameMangleExpr exprs
    return (EApp lvalue' exprs')
nameMangleExpr (Neg expr) = do
    expr' <- nameMangleExpr expr
    return (Neg expr')
nameMangleExpr (Not expr) = do
    expr' <- nameMangleExpr expr
    return (Not expr')
nameMangleExpr (EMul expr1 mulOp expr2) = do
    expr1' <- nameMangleExpr expr1
    expr2' <- nameMangleExpr expr2
    return (EMul expr1' mulOp expr2')
nameMangleExpr (EAdd expr1 addOp expr2) = do
    expr1' <- nameMangleExpr expr1
    expr2' <- nameMangleExpr expr2
    return (EAdd expr1' addOp expr2')
nameMangleExpr (ERel expr1 relOp expr2) = do
    expr1' <- nameMangleExpr expr1
    expr2' <- nameMangleExpr expr2
    return (ERel expr1' relOp expr2')
nameMangleExpr (EAnd expr1 expr2) = do
    expr1' <- nameMangleExpr expr1
    expr2' <- nameMangleExpr expr2
    return (EAnd expr1' expr2')
nameMangleExpr (EOr expr1 expr2) = do
    expr1' <- nameMangleExpr expr1
    expr2' <- nameMangleExpr expr2
    return (EOr expr1' expr2')
nameMangleExpr expr = return expr