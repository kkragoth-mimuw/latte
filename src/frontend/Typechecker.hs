module Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Bool
import Text.Printf

import System.IO.Unsafe (unsafePerformIO) -- only debug

import AbsLatte
import PrintLatte

import Utils

type TCM a = (ExceptT TypecheckErrorWithLogging (Reader TCEnv)) a

data TCEnv = TCEnv {
    typesMap :: Map.Map Ident (Type, Integer),
    classes :: Map.Map Ident Class,
    level :: Integer,
    currentFunctionReturnType :: Maybe Type,
    currentClass :: Maybe Class,
    classesCheckForDuplicates :: S.Set Ident
}

initTCEnv = TCEnv {
    typesMap = initTypesMap,
    classes = Map.empty,
    level = 0,
    currentFunctionReturnType = Nothing,
    currentClass = Nothing,
    classesCheckForDuplicates = S.empty
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
    Boolean  -> "Boolean"
    Void  -> "void"
    (ClassType (Ident c) -> "class " ++ c

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
        let classesMap = ( createClassMapFromTopDefs topdefs)

        newEnv <- local (\env -> env { classes = classesMap }) (fillTopDefsInformation topdefs)
        local (const newEnv) (mapM_ typecheckTopDef topdefs)
        local (const newEnv) typecheckMainBlock
        return ()

fillTopDefsInformation :: [TopDef] -> TCM TCEnv
fillTopDefsInformation [] = ask
fillTopDefsInformation ((FnDef fnType (Ident fnNameNotNormalized) args (Block stmts)):xs) = do
    let fnName = Ident (takeWhile (\c -> c /= '\'') fnNameNotNormalized)
    checkIfIsAlreadyDeclaredATCurrentLevel fnName
    env <- fillTopDefsInformation xs

    case Map.lookup fnName (typesMap env) of
        Just _ -> throwError $ initTypecheckError $ TCFunctionRedeclaration fnName
        Nothing -> do
                let funcType = Fun fnType (Prelude.map argToType args)
                let newTypesMap = Map.insert fnName (funcType, level env) (typesMap env)
                let newEnv = env { typesMap = newTypesMap }

                return newEnv
fillTopDefsInformation ((ClassDef cName classPoles):xs) = do
    env <- fillTopDefsInformation xs
    case S.member cName (classesCheckForDuplicates env) of
        True -> throwError $ initTypecheckError $ TCClassRedeclaration cName
        False -> return env { classesCheckForDuplicates = S.insert cName (classesCheckForDuplicates env)}
fillTopDefsInformation ((ClassDefExt cName bName _):xs) = do
    env <- fillTopDefsInformation xs
    case Map.lookup (bName) (classes env) of
        Nothing -> throwError $ initTypecheckError $ TCUndeclaredClass bName
        Just _ ->  case S.member cName (classesCheckForDuplicates env) of
                True -> throwError $ initTypecheckError $ TCClassRedeclaration cName
                False -> return env { classesCheckForDuplicates = S.insert cName (classesCheckForDuplicates env)}
-- typecheckAllMethods :: TCM ()
-- typecheckAllMethods = do
--     classesMap <- asks classes
    
--     forM_ (Map.elems classesMap) typecheckClassMethods 

-- typecheckClassMethods :: Class -> TCM ()
-- typecheckClassMethods c = do
--     env <- ask
--     let newEnv = env { currentClass = Just c }

--     forM_ (classMethods c) (\classMethod -> 
--                 local (const newEnv) (
--                         typecheckTopDef (FnDef 
--                             (classMethodReturnType classMethod)
--                             (classMethodName classMethod)
--                             (classMethodArgs classMethod)
--                             (classMethodBlock classMethod)
--                         )
--                 )
--         )

typecheckMainBlock :: TCM ()
typecheckMainBlock = do
    env <- ask

    case Map.lookup (Ident "main") (typesMap env) of
        Nothing -> throwError $ initTypecheckError $ TCNoMain
        Just ((Fun Int _), _) -> return ()
        _ -> throwError $ initTypecheckError $ TCMainInvalidReturnType


typecheckTopDef :: TopDef -> TCM ()
typecheckTopDef (FnDef fnType (Ident fnNameNotNormalized) args (Block stmts)) = do 
    let fnName = Ident (takeWhile (\c -> c /= '\'') fnNameNotNormalized)
    
    let argsNames = map (\(Arg _ ident) -> ident) args
    let argsTypes = map (\(Arg type' _) -> type') args

    unless (allUnique argsNames)
        (throwError $ initTypecheckError $ TCRepeatedArguments fnName)

    unless (Void `notElem` argsTypes)
        (throwError $ initTypecheckError $ TCVoidArgument fnName)

    let (Ident name) = fnName
    if (name == "main" && length args /= 0) then
        throwError $ initTypecheckError $ TCMainInvalidArgs
    else do
        env <- ask
        let newEnvForFunction = Prelude.foldr updateEnv env args

        local (const $ indicateReturnType (increaseLevel newEnvForFunction) fnType) (typecheckStmts stmts)

        return ()
typecheckTopDef _ = return ()


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
typecheckDecl (Decl Void ((Init ident _):_)) = throwError $ initTypecheckError $ TCVoidType ident
typecheckDecl (Decl Void ((NoInit ident):_)) = throwError $ initTypecheckError $ TCVoidType ident
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
typecheckStmt (Ass lvalue expr) = do
    lvalueType <- extractLValueType lvalue
    rvalueType <- typecheckExpr expr

    when (lvalueType /= rvalueType)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType rvalueType lvalueType)

typecheckStmt (Incr lvalue) = do
    lvalueType <- extractLValueType lvalue

    unless (lvalueType == Int)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType lvalueType Int)

typecheckStmt (Decr lvalue) = do
    lvalueType <- extractLValueType lvalue

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

    unless (exprType == Boolean)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType Boolean)

    typecheckStmt stmtTrue
    typecheckStmt stmtFalse

typecheckStmt (While expr stmt) = do
    conditionType <- typecheckExprWithErrorLogging expr

    unless (conditionType == Boolean)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType conditionType Boolean)

    typecheckStmt stmt

typecheckStmt (SExp expr) = typecheckExpr expr >> return ()

typecheckStmt decl = typecheckStmtOrDeclaration decl >> return ()

typecheckExprWithErrorLogging :: Expr -> TCM Type
typecheckExprWithErrorLogging expr = typecheckExpr expr `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError expr))

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EString _) = return Str
typecheckExpr (ELitInt i) = if (i > 2147483647 || i < -2147483648) then (throwError $ initTypecheckError $ TCLiteralOverflow i) else return Int
typecheckExpr (ELitTrue) = return Boolean
typecheckExpr (ELitFalse) = return Boolean
typecheckExpr (EOr expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Boolean, Boolean) -> return Boolean
        (Boolean, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Boolean
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Boolean
typecheckExpr (EAnd expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Boolean, Boolean) -> return Boolean
        (Boolean, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Boolean
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Boolean
typecheckExpr (Not expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Boolean -> return Boolean
        x    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Boolean
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
        (Boolean, Boolean, EQU) -> return Boolean
        (Boolean, Boolean, NE) -> return Boolean
        (Str, Str, EQU) -> return Boolean
        (Str, Str, NE) -> return Boolean
        (Int, Int, _) -> return Boolean
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
typecheckExpr(EApp lvalue exprs) = do
    name <- getFuncNameFromLValue lvalue
    funcType <- extractVariableType name
    typecheckFuncApplication funcType exprs
typecheckExpr (ELValue lvalue) = do
    type' <- extractLValueType lvalue
    return type'
getFuncNameFromLValue :: LValue -> TCM Ident
getFuncNameFromLValue  (LValue ident) = do
    return ident
getFuncNameFromLValue  _ = error "todo"

extractLValueType :: LValue -> TCM Type
extractLValueType (LValue ident) = do
    extractVariableType ident
extractLValueType (LValueClassField lvalue ident) = do
    cType <- extractLValueType lvalue
    case cType of
        ClassType cIdent -> do
            env <- ask
            case Map.lookup (cIdent) (classes env) of
                Nothing -> throwError $ initTypecheckError $ TCUndeclaredClass cIdent
                Just c -> case find (\(ClassFieldDef _ cFieldIdent)  -> cFieldIdent == ident) (classFields c) of
                    Nothing -> throwError $ initTypecheckError $ TCClassDoesntHaveField (cIdent) (ident)
                    Just (ClassFieldDef classFieldType _) -> return classFieldType
        _ ->  throwError $ initTypecheckError $ TCAccessOnNonClass 
extractLValueType _ = error "TODO"


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

allUnique xs = foldr go (\s -> s `seq` True) xs S.empty
    where
        go x k s
            | S.member x s = False
            | otherwise = k $! S.insert x s

initTypesMap = Map.fromList [
        (Ident "printInt", ((Fun Void ([Int])), -1)),
        (Ident "printString", ((Fun Void ([Str])), -1)),
        (Ident "error", ((Fun Void []), -1)),
        (Ident "readInt", ((Fun Int []), -1)),
        (Ident "readString", ((Fun Str []), -1))    
    ]

data TypecheckError = TCInvalidTypeExpectedType Type Type
                    | TCInvalidTypeExpectedTypes Type [Type]
                    | TCInvalidFunctionAppTypes [Type] [Type]
                    | TCInvalidNumberOfArguments
                    | TCUndeclaredVariable Ident
                    | TCNoMain 
                    | TCNotLValue
                    | TCRedeclaration Ident
                    | TCFunctionRedeclaration Ident
                    | TCReturn
                    | TCMainInvalidArgs
                    | TCMainInvalidReturnType
                    | TCLiteralOverflow Integer
                    | TCRepeatedArguments Ident
                    | TCVoidArgument Ident
                    | TCVoidType Ident
                    | TCClassRedeclaration Ident
                    | TCAccessOnNonClass
                    | TCUndeclaredClass Ident
                    | TCClassDoesntHaveField Ident Ident
                    | TCDebug String

instance Show TypecheckError where
    show (TCInvalidTypeExpectedType type' allowedType)   = printf "Invalid type: %s. Expected: %s" (prettyShowType type') (prettyShowType allowedType)
    show (TCInvalidTypeExpectedTypes type' allowedTypes) = printf "Invalid type: %s. Expected %s" (prettyShowType type') (intercalate " or " (map prettyShowType allowedTypes))
    show (TCRedeclaration (Ident ident))                 = printf "Tried to redeclare: %s" (show ident)
    show (TCFunctionRedeclaration (Ident ident))         = printf "Tried to redeclare function: %s" (show ident)
    show (TCMainInvalidArgs)                             = "Main shouldn't take any arguments!"
    show (TCMainInvalidReturnType)                       = "Main should only return int"
    show TCNotLValue                                     = "Not lvalue"
    show TCInvalidNumberOfArguments                      = "Passed invalid number of arguments to function"
    show (TCDebug str)                                   = printf "%s" (show str)
    show TCReturn                                        = "return error"
    show TCNoMain                                        = "Program has no main function"
    show (TCLiteralOverflow i)                           = printf "Constant %s is too large" (show i)
    show (TCUndeclaredVariable (Ident ident))            = printf "Use of undeclared variable %s" (show ident)
    show (TCRepeatedArguments (Ident ident))             = printf ("Function %s has repeated arguments") (show ident)
    show (TCVoidArgument (Ident ident))                  = printf ("Argument of function %s has invalid type void") (show ident)
    show (TCVoidType (Ident ident))                      = printf ("Tried to declare %s as void") (show ident)
    show (TCClassRedeclaration (Ident ident))            = printf ("Tried to redeclare class %s") (show ident)
    show (TCAccessOnNonClass)                            = printf ("Tried to access member of type which isn't class")
    show (TCUndeclaredClass (Ident ident))               = printf ("Non existing class %s") (show ident)
    show (TCClassDoesntHaveField (Ident c) (Ident f))    = printf ("Class %s doesn't have field %s") (show c) (show f)
    show _ = ""
