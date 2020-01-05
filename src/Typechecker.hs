module Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Char
import Data.List
import Text.Printf

import AbsLatte
import PrintLatte

type TCM a = (ExceptT TypecheckErrorWithLogging (Reader TCEnv)) a

data TCEnv = TCEnv {
    typesMap                  :: Map.Map Ident (Type, Integer),
    level :: Integer,
    currentFunctionReturnType :: Maybe Type
}

initTCEnv = TCEnv {
    typesMap = Map.fromList [
        (Ident "printInt", ((Fun Void ([Int])), -1)),
        (Ident "printString", ((Fun Void ([Str])), -1)),
        (Ident "error", ((Fun Void []), -1)),
        (Ident "readInt", ((Fun Int []), -1)),
        (Ident "readString", ((Fun Str []), -1))    
    ],
    level = 0,
    currentFunctionReturnType = Nothing
}

updateEnv :: Arg -> TCEnv -> TCEnv
updateEnv (Arg type' ident) env = env { typesMap = updatedTypesMap }
    where updatedTypesMap = Map.insert ident (type', level env) (typesMap env)


extractVariableType :: Ident -> TCM Type
extractVariableType ident = do
    env <- ask

    case Map.lookup ident $ typesMap env of
        Just (type', _) -> return type'
        _ -> throwError $ initTypecheckError $ TCUndeclaredVariable ident


checkIfIsAlreadyDeclaredATCurrentLevel :: Ident -> TCM ()
checkIfIsAlreadyDeclaredATCurrentLevel ident = do
    env <- ask

    case Map.lookup ident $ typesMap env of
        Just (_, levelDeclared) | levelDeclared >= (level env) -> throwError $ initTypecheckError (TCRedeclaration ident)
        _ -> return ()

increaseLevel :: TCEnv -> TCEnv
increaseLevel env = env { level = (level env) + 1}

indicateReturnType :: TCEnv -> Type -> TCEnv
indicateReturnType env type' = env { currentFunctionReturnType = Just (type') }

data TypecheckError = TCInvalidTypeExpectedType Type Type
                    | TCInvalidTypeExpectedTypes Type [Type]
                    | TCInvalidFunctionAppTypes [Type] [Type]
                    | TCInvalidNumberOfArguments
                    | TCUndeclaredVariable Ident
                    | TCNotLValue
                    | TCRedeclaration Ident
                    | TCReturn
                    | TCMainInvalidArgs
                    | TCMainInvalidReturnType
                    | TCDebug String

instance Show TypecheckError where
    show (TCInvalidTypeExpectedType type' allowedType)   = printf "Invalid type: %s. Expected: %s" (prettyShowType type') (prettyShowType allowedType)
    show (TCInvalidTypeExpectedTypes type' allowedTypes) = printf "Invalid type: %s. Expected %s" (prettyShowType type') (intercalate " or " (map prettyShowType allowedTypes))
    show (TCRedeclaration (Ident ident))                 = printf "Tried to redeclare: %s" (show ident)
    show (TCMainInvalidArgs)                             = "Main shouldn't take any arguments!"
    show (TCMainInvalidReturnType)                       = "Main should only return int"
    show TCNotLValue                                     = "Not lvalue"
    show TCInvalidNumberOfArguments                      = "Passed invalid number of arguments to function"
    show (TCDebug str)                                   = printf "%s" (show str)
    show TCReturn                                        = "return error"
    show _ = ""

data TypecheckErrorWithLogging = TypecheckErrorWithLogging TypecheckError Integer [String] deriving (Show)


defaultLevelOfLogging = 7

initTypecheckError :: TypecheckError -> TypecheckErrorWithLogging
initTypecheckError error = TypecheckErrorWithLogging error defaultLevelOfLogging []


appendLogToTypecheckError :: (Print a) => TypecheckErrorWithLogging -> a -> TypecheckErrorWithLogging
appendLogToTypecheckError typecheckErrorWithLogging@(TypecheckErrorWithLogging error loggingLevel msg) location | loggingLevel <= 0 = typecheckErrorWithLogging
appendLogToTypecheckError                           (TypecheckErrorWithLogging error loggingLevel msg) location = TypecheckErrorWithLogging error (loggingLevel - 1) (msg ++ [printTree location])


pprintTypecheckerErrorMsg :: TypecheckErrorWithLogging -> IO ()
pprintTypecheckerErrorMsg wholeMsg@(TypecheckErrorWithLogging error _ stack) = do
    putStrLn ("Encountered error " ++ show error)
    mapM_ (\line -> putStrLn ("\nFound in:\n " ++  trim line))  stack


prettyShowType :: Type -> String
prettyShowType type_ = case type_ of
    Int   -> "int"
    Str   -> "string"
    Bool  -> "bool"
    Void  -> "void"

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight str      | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

trim :: String -> String
trim = trimLeft . trimRight

runTypecheck :: (Typecheckable program) => TCEnv -> program -> (Either TypecheckErrorWithLogging ())
runTypecheck env program = runReader (runExceptT (typecheckProgram program)) env

class Typecheckable f where
    typecheckProgram :: f -> TCM ()

instance Typecheckable Program where
    typecheckProgram (Program topdefs) = do
        env <- fillFunctionsInformations topdefs
        local (const env) (mapM_ typecheckTopDef topdefs)
        local (const env) typecheckMainBlock
        return ()

fillFunctionsInformations :: [TopDef] -> TCM TCEnv
fillFunctionsInformations [] = ask
fillFunctionsInformations ((FnDef fnType fnName args (Block stmts)):xs) = do
    checkIfIsAlreadyDeclaredATCurrentLevel fnName
    env <- fillFunctionsInformations xs
    let funcType = Fun fnType (Prelude.map argToType args)
    let newTypesMap = Map.insert fnName (funcType, level env) (typesMap env)
    let newEnv = env { typesMap = newTypesMap }

    return newEnv
    

typecheckMainBlock :: TCM ()
typecheckMainBlock = do
    env <- ask

    case Map.lookup (Ident "main") (typesMap env) of
        Nothing -> return ()
        Just ((Fun Int _), _) -> return ()
        _ -> throwError $ initTypecheckError $ TCMainInvalidReturnType

-- typecheckTopDefs :: [TopDef] -> TCM ()
-- typecheckTopDefs [] = ()
-- typecheckTopDefs (x:xs) = do
--     typecheckTopDef x
--     typecheckTopDefs xs

typecheckTopDef :: TopDef -> TCM ()
typecheckTopDef (FnDef fnType fnName args (Block stmts)) = do 
    -- checkIfIsAlreadyDeclaredATCurrentLevel fnName
    let (Ident name) = fnName
    if (name == "main" && length args /= 0) then
        throwError $ initTypecheckError $ TCMainInvalidArgs
    else do
        env <- ask
        -- let funcType = Fun fnType (Prelude.map argToType args)
        -- let newTypesMap = Map.insert fnName (funcType, level env) (typesMap env)
        -- let newEnv = env { typesMap = newTypesMap }
        let newEnvForFunction = Prelude.foldr updateEnv env args

        local (const $ indicateReturnType (increaseLevel newEnvForFunction) fnType) (typecheckStmts stmts)

        return ()


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

typecheckDeclWithLogging :: Stmt -> TCM TCEnv
typecheckDeclWithLogging stmt = typecheckDecl stmt `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError stmt))

typecheckDecl :: Stmt -> TCM TCEnv
typecheckDecl (Decl type' []) = ask
typecheckDecl (Decl type' (item:items)) = do
    env <- typecheckDeclItem type' item
    local (const env) (typecheckDecl (Decl type' items))


typecheckDeclItem :: Type -> Item -> TCM TCEnv
typecheckDeclItem type' (Init ident expr) = do
    exprType <- typecheckExpr expr

    checkIfIsAlreadyDeclaredATCurrentLevel ident

    when (type' /= exprType)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType type')

    env <- ask

    let updatedTypesMap = Map.insert ident (type', level env) (typesMap env)

    return env { typesMap = updatedTypesMap}

typecheckDeclItem type' (NoInit ident) = do
    checkIfIsAlreadyDeclaredATCurrentLevel ident

    env <- ask

    let updatedTypesMap = Map.insert ident (type', level env) (typesMap env)

    return env { typesMap = updatedTypesMap}

typecheckStmtWithLogging :: Stmt -> TCM ()
typecheckStmtWithLogging stmt = typecheckStmt stmt `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError stmt))

typecheckStmt :: Stmt -> TCM ()
typecheckStmt Empty = return ()
typecheckStmt (BStmt (Block stmts)) = local increaseLevel (typecheckStmts stmts)
-- typecheckStmt (Decl) -> typecheckDecl
typecheckStmt (Ass ident expr) = do
    lvalueType <- extractVariableType ident
    rvalueType <- typecheckExpr expr

    when (lvalueType /= rvalueType)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType rvalueType lvalueType)

typecheckStmt (Incr ident) = do
    lvalueType <- extractVariableType ident

    unless (lvalueType == Int)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType lvalueType Int)

typecheckStmt (Decr ident) = do
    lvalueType <- extractVariableType ident

    unless (lvalueType == Int)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType lvalueType Int)

typecheckStmt VRet = do
    env <- ask

    case currentFunctionReturnType env of
        Nothing -> throwError $ initTypecheckError TCReturn
        Just Void -> return ()
        Just t -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType Void t

typecheckStmt (Ret expr) = do
        exprType <- typecheckExpr expr
    
        env <- ask
    
        case currentFunctionReturnType env of
            Nothing -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType Void
            Just a | exprType == a -> return ()
            Just t -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType t
        

typecheckStmt (Cond expr stmt) = typecheckStmt (CondElse expr stmt (Empty))

typecheckStmt (CondElse expr stmtTrue stmtFalse) = do
    exprType <- typecheckExprWithErrorLogging expr

    unless (exprType == Bool)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType Bool)

    typecheckStmt stmtTrue
    typecheckStmt stmtFalse

typecheckStmt (While expr stmt) = do
    conditionType <- typecheckExprWithErrorLogging expr

    unless (conditionType == Bool)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType conditionType Bool)

    typecheckStmt stmt

typecheckStmt (SExp expr) = typecheckExpr expr >> return ()

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
typecheckExpr (ERel exprLeft op exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right, op) of
        (Bool, Bool, EQU) -> return Bool
        (Bool, Bool, NE) -> return Bool
        (Int, Int, _) -> return Bool
        (Int, x, _)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _, _)     -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr(EAdd expr1 addop expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right, addop) of
        (Str, Str, Plus) -> return Str
        (Int, Int, _)    -> return Int
        (Str, x, Plus)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (Int, x, _)      -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _, _)        -> throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes x [Int, Str]

typecheckExpr(EVar ident) = do 
    extractVariableType ident

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


isStmtDeclaration :: Stmt -> Bool
isStmtDeclaration stmt = case stmt of
    (Decl _  _) -> True
    _          -> False