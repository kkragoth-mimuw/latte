{-# LANGUAGE LambdaCase #-}

module Frontend.Typechecker.Typecheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Map as Map

import AbsLatte

import Frontend.Typechecker.Errors
import Frontend.Typechecker.TCM
import Frontend.Typechecker.Environment
import Frontend.Typechecker.EnvironmentUtils

runTypecheck :: (Typecheckable program) => TCEnv -> program -> (Either TypecheckErrorWithLogging ())
runTypecheck env program = runReader (runExceptT (typecheckProgram program)) env

class Typecheckable f where
    typecheckProgram :: f -> TCM ()

instance Typecheckable Program where
    typecheckProgram (Program topdefs) = typecheckTopDefs topdefs >> return ()

typecheckTopDefs :: [TopDef] -> TCM TCEnv
typecheckTopDefs [] = ask
typecheckTopDefs (x:xs) = do
    env <- typecheckTopDef x
    local (const env) (typecheckTopDefs xs)

typecheckTopDef :: TopDef -> TCM TCEnv
typecheckTopDef (FnDef fnType fnName args (Block stmts)) = do 
    checkIfIsAlreadyDeclaredAtCurrentLevel fnName
    env <- ask
    let funcType = Fun fnType (Prelude.map argToType args)
    let newTypesMap = Map.insert fnName (funcType, level env) (typesMap env)
    let newEnv = env { typesMap = newTypesMap }
    let newEnvForFunction = Prelude.foldr updateEnv newEnv args

    return newEnv


argToType :: Arg -> Type
argToType (Arg type' _) = type'

typecheckStmts :: [Stmt] -> TCM ()
typecheckStmts [] = return ()
typecheckStmts (x:xs) = do
    env <- typecheckStmtOrDeclaration x
    local (const env) (typecheckStmts xs)

typecheckStmtOrDeclaration :: Stmt -> TCM TCEnv
typecheckStmtOrDeclaration stmt =
    if isStmtDeclaration stmt then
        typecheckDeclWithLogging stmt
    else
        typecheckStmtWithLogging stmt >> ask

typecheckStmtWithLogging :: Stmt -> TCM ()
typecheckStmtWithLogging stmt = typecheckStmt stmt `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError stmt))

typecheckStmt :: Stmt -> TCM ()
typecheckStmt Empty = return ()
typecheckStmt (Cond expr block@(Block blockTrue)) = typecheckStmt (CondElse expr block (Block []))

typecheckStmt (CondElse expr (Block blockTrue) (Block blockFalse)) = do
    exprType <- typecheckExprWithErrorLogging expr

    unless (exprType == Bool)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType Bool)

    typecheckStmts blockTrue
    typecheckStmts blockFalse


typecheckExprWithErrorLogging :: Expr -> TCM Type
typecheckExprWithErrorLogging expr = typecheckExpr expr `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError expr))

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EString _) = return Str
typecheckExpr (ELitInt _) = return Int
typecheckExpr (ELitTrue) = return Bool
typecheckExpr (ELitFalse) = return Bool
typecheckExpr (EOr expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (EAnd expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Not expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Bool -> return Bool
        x    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Neg expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Int -> return Int
        x   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (EMul exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (Int, Int) -> return Int
        (Int, x)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _)     ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (ERel exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (Str, Str) -> return Bool
        (Int, Int) -> return Bool
        (Int, x)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (Str, x)   ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (x, _)     -> throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes x [Int, Str]
typecheckExpr(EAdd expr1 addop expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right, addop) of
        (Str, Str, Plus) -> return Str
        (Int, Int, _)    -> return Int
        (Str, x, Plus)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (Int, x, _)      -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _, _)        -> throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes x [Int, Str]

typecheckExpr(EVar ident) = ident >>= extractVariableType

typecheckExpr(EApp ident exprs) = do
    funcType <- extractVariableType ident
    typecheckFuncApplication funcType exprs

typecheckFuncApplication :: Type -> [Expr] -> TCM Type
typecheckFuncApplication (Fun returnType argTypes) exprs = do
    unless (length exprs == length argTypes)
        (throwError $ initTypecheckError $ TCInvalidNumberOfArguments)

    exprTypes <- mapM typecheckExpr exprs

    let allCorrectTypes = Prelude.map (\(a, b) -> (a == b, (a, b))) (zip argTypes exprTypes)
    
    case find (\(eq, _) -> not eq) allCorrectTypes of
        Just (_, (a, b)) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType a b
        Nothing -> return returnType


typecheckExpr2 :: Expr -> Expr -> TCM (Type, Type)
typecheckExpr2 leftExpr rightExpr = do
    leftType <- typecheckExprWithErrorLogging leftExpr
    rightType <- typecheckExprWithErrorLogging rightExpr
    return (leftType, rightType)


evalLValueToIdent :: Expr -> TCM Ident
evalLValueToIdent (EVar ident) = ident
evalLValueToIdent loc           = throwError $ initTypecheckError $ TCNotLValue


-- evalLValue :: LValue -> TCM Ident
-- evalLValue (LValue n) = return n


isStmtDeclaration :: Stmt -> Bool
isStmtDeclaration stmt = case stmt of
    (Decl _  _) -> True
    _          -> False