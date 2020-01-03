{-# LANGUAGE LambdaCase #-}

-- https://buildmedia.readthedocs.org/media/pdf/mapping-high-level-constructs-to-llvm-ir/latest/mapping-high-level-constructs-to-llvm-ir.pdf
-- /usr/local/opt/llvm/bin

module LLVM.Compiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import AbsLatte 

type GenM a = (ReaderT Env (StateT Store IO) ) a

runCompiler :: (Compilable program) => program -> IO String
runCompiler program = do
    (_, store) <- runStateT (runReaderT (compile program) initEnv) initStore
    return $ show store

class Compilable f  where
    compile :: f -> GenM ()

data Env = Env {
    vars :: Map.Map Ident LLVMVariable
}

initEnv = Env {
    vars = Map.empty
}

data Store = Store {
    currentFunction :: Ident,
    currentLabel :: Integer,
    functions :: Map.Map Ident Type,
    labelCounter :: Integer,
    registerCounter :: Integer,
    functionBlocks :: Map.Map Ident BlockMap,
    stringsMap :: Map.Map String Integer
} deriving (Show)

initStore = Store {
    currentFunction = Ident "",
    currentLabel = 0,
    functions = Map.empty,
    labelCounter = 0,
    registerCounter = 0,
    functionBlocks = Map.empty,
    stringsMap = Map.empty
}

type BlockMap = Map.Map Integer LLVMBlock

data LLVMBlock = LLVMBlock {
    label :: Integer,
    code :: [LLVMInstruction],
    inEdges :: [Integer],
    outEdges :: [Integer]
} deriving (Show)


data Op = AddBinOp AddOp | MulBinOp MulOp | RelBinOp RelOp | AndOp | OrOp | Xor deriving (Show)

data LLVMInstruction = Alloca LLVMVariable
    | Operation LLVMVariable Op LLVMVariable LLVMVariable
    | MemoryStore LLVMVariable LLVMVariable
    | Load LLVMVariable LLVMVariable
    | ReturnVoid
    | Return LLVMVariable
    | Branch Integer
    | Call LLVMVariable Ident [LLVMVariable]
    | CallVoid Ident [LLVMVariable]
    | BranchConditional LLVMVariable Integer Integer deriving (Show)

data LLVMAddress = LLVMAddressVoid
    | LLVMAddressImmediate Integer
    | LLVMAddressString String Integer
    | LLVMAddressRegister Integer deriving (Show)

data LLVMVariable = LLVMVariable {
    type' :: Type,
    address :: LLVMAddress,
    blockLabel :: Integer,
    ident :: Maybe Ident
} deriving (Show)


instance Compilable Program where
    compile (Program topdefs) = do
        forM_ topdefs fillFunctionsInformation
        compileFnDefs topdefs
        return ()


fillFunctionsInformation :: TopDef -> GenM ()
fillFunctionsInformation (FnDef type' ident args _) = do
    modify (\store -> store {
        functions = Map.insert ident (Fun type' (map (\(Arg t _) -> t) args)) (functions store)
    })

emit :: LLVMInstruction -> GenM ()
emit instruction = do
    store <- get
    let blockLabel = currentLabel store
    emitInSpecificBlock blockLabel instruction

emitInSpecificBlock :: Integer -> LLVMInstruction -> GenM ()
emitInSpecificBlock blockLabel instruction = do
    store <- get
    let blockMap = fromJust $ Map.lookup (currentFunction store) (functionBlocks store)
    case Map.lookup blockLabel (blockMap) of
        Just block -> let newMap = Map.insert blockLabel (LLVMBlock { label = label block, code = (code block) ++ [instruction]}) (blockMap) in
            modify (\store -> store {
                functionBlocks = Map.insert (currentFunction store) (newMap) (functionBlocks store)
            })
        Nothing -> let newMap = Map.insert blockLabel (LLVMBlock { label = blockLabel, code = [instruction]}) (blockMap) in
            modify (\store -> store {
                functionBlocks = Map.insert (currentFunction store) (newMap) (functionBlocks store)
            })

    liftIO $ putStrLn $ "emit"

getNewLabel :: GenM Integer
getNewLabel = do
    modify (\store ->
        store { 
            labelCounter = (labelCounter store) + 1,
            currentLabel = (labelCounter store) + 1
        })
    store <- get
    return $ labelCounter store

compileFnDefs :: [TopDef] -> GenM String
compileFnDefs [] = return ""
compileFnDefs (x:xs) = do
    compileFnDef x
    compileFnDefs xs

compileFnDef :: TopDef -> GenM String
compileFnDef (FnDef type' ident args block) = do
    modify (\store -> store {
        currentFunction = ident,
        currentLabel = 0,
        labelCounter = 0,
        functionBlocks = Map.insert ident Map.empty (functionBlocks store)
    })
    compileBlock block
    return ""

compileBlock :: Block -> GenM ()
compileBlock (Block stmts) = do
    store <- get
    let previousBlockLabel = currentLabel store
    newBlockLabel <- getNewLabel

    emitInSpecificBlock previousBlockLabel (Branch newBlockLabel)
    compileStmts stmts

    return ()

compileStmts :: [Stmt] -> GenM Env
compileStmts [] = ask
compileStmts (stmt:stmts) = do
    env <- compileStmt stmt
    local (const env) (compileStmts stmts)

compileStmt :: Stmt -> GenM Env
compileStmt (Empty) = ask
compileStmt (BStmt block) = do
    compileBlock block
    ask
compileStmt (Decl type' items) = compileDecls type' items
compileStmt (Ass ident expr) = do
    lhs <- getVar ident
    rhs <- compileExpr expr
    emit (MemoryStore rhs lhs)
    ask
compileStmt (Incr ident) = compileStmt (Ass ident (EAdd (EVar ident) Plus (ELitInt 1)))
compileStmt (Decr ident) = compileStmt (Ass ident (EAdd (EVar ident) Minus (ELitInt 1)))
compileStmt (Ret expr) = do
    val <- compileExpr expr
    emit $ Return val
    ask
compileStmt (VRet) = do
    emit ReturnVoid
    ask
compileStmt (SExp expr) = do
    _ <- compileExpr expr
    ask
compileStmt (Cond expr stmt) = do
    condition <- compileExpr expr
    previousLabel <- gets currentLabel
    ifTrueStmtsLabel <- getNewLabel
    _ <- compileStmt stmt
    afterIfBlock <- getNewLabel
    emitInSpecificBlock previousLabel (BranchConditional condition ifTrueStmtsLabel afterIfBlock)
    emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)
    ask
compileStmt (CondElse expr stmtTrue stmtFalse) = do
    condition <- compileExpr expr
    previousLabel <- gets currentLabel
    ifTrueStmtsLabel <- getNewLabel
    _ <- compileStmt stmtTrue
    ifFalseStmtsLabel <- getNewLabel
    _ <- compileStmt stmtFalse
    afterIfBlock <- getNewLabel
    emitInSpecificBlock previousLabel (BranchConditional condition ifTrueStmtsLabel ifFalseStmtsLabel)
    emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)
    emitInSpecificBlock ifFalseStmtsLabel (Branch afterIfBlock)
    ask
compileStmt (While expr stmt) = do
    preConditionLabel <- gets currentLabel
    conditionLabel <- getNewLabel
    emitInSpecificBlock preConditionLabel (Branch conditionLabel)
    condition <- compileExpr expr
    ifTrueBodyLabel <- getNewLabel
    _ <- compileStmt stmt
    emit (Branch conditionLabel)
    afterWhileLabel <- getNewLabel
    emitInSpecificBlock conditionLabel (BranchConditional condition ifTrueBodyLabel afterWhileLabel)
    ask

compileDecls :: Type -> [Item] -> GenM Env
compileDecls type' [] = ask
compileDecls type' (item:items) = do
    env <- compileDecl type' item
    newEnv <- local (const env) (compileDecls type' items)
    return newEnv

compileDecl :: Type -> Item -> GenM Env
compileDecl type' (NoInit ident) = do
    rhs <- defaultVariable type' 
    variableRegister <- getNextRegisterCounter
    env <- ask
    store <- get
    let allocaVar = (LLVMVariable {
        type' = type',
        address = LLVMAddressRegister variableRegister,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Alloca allocaVar)
    emit (MemoryStore rhs allocaVar)
    return $ env { vars = Map.insert ident allocaVar (vars env )}
compileDecl type' (Init ident expr) = do
    rhs <- compileExpr expr
    variableRegister <- getNextRegisterCounter
    env <- ask
    store <- get
    let allocaVar = (LLVMVariable {
        type' = type',
        address = LLVMAddressRegister variableRegister,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Alloca allocaVar)
    emit (MemoryStore rhs allocaVar)
    return $ env { vars = Map.insert ident allocaVar (vars env )}

defaultVariable :: Type -> GenM LLVMVariable
defaultVariable Int = do
    store <- get
    return $ (LLVMVariable {
        type' = Int,
        address = LLVMAddressImmediate 0,
        blockLabel = currentLabel store,
        ident = Nothing
    })
defaultVariable Bool = do
    store <- get
    return $ (LLVMVariable {
        type' = Int,
        address = LLVMAddressImmediate 0,
        blockLabel = currentLabel store,
        ident = Nothing
    })
defaultVariable Str = error "defaultStr not implemented"


-- data Expr
--     = EVar Ident
--     | ELitInt Integer
--     | ELitTrue
--     | ELitFalse
--     | EApp Ident [Expr]
--     | EString String
--     | Neg Expr
--     | Not Expr
--     | EMul Expr MulOp Expr
--     | EAdd Expr AddOp Expr
--     | ERel Expr RelOp Expr
--     | EAnd Expr Expr
--     | EOr Expr Expr
--   deriving (Eq, Ord, Show, Read)
compileExpr :: Expr -> GenM LLVMVariable
compileExpr (EVar ident) = do
    store <- get
    varsMap <- asks vars
    let var = fromJust $ Map.lookup ident varsMap
    newRegisterNumber <- getNextRegisterCounter
    let registerVar = (LLVMVariable {
        type' = type' var,
        address = LLVMAddressRegister newRegisterNumber,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Load registerVar var)
    return registerVar
compileExpr (ELitInt i) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = Int,
        address = LLVMAddressImmediate i,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr(ELitTrue) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = Bool,
        address = LLVMAddressImmediate 1,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr(ELitFalse) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = Bool,
        address = LLVMAddressImmediate 0,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr (EApp ident exprs) = do
    args <- mapM compileExpr exprs
    store <- get
    let func = fromJust $ Map.lookup ident (functions store)
    case func of
        (Fun Void _) -> do
            emit (CallVoid ident args)
            return (LLVMVariable {
                type' = Void,
                address = LLVMAddressVoid,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
        (Fun type' _) -> do
            newRegister <- getNextRegisterCounter
            let result = (LLVMVariable {
                type' = type',
                address = LLVMAddressRegister newRegister,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
            emit (Call result ident args)
            return result
compileExpr (EString s) = do
    store <- get
    case Map.lookup s (stringsMap store) of
        Just i -> do
            return (LLVMVariable {
                type' = Str,
                address = LLVMAddressString s i,
                blockLabel = currentLabel store,
                ident = Nothing
            })
        Nothing -> do
            let newLength = toInteger $ (Map.size (stringsMap store)) + 1
            modify (\store -> (store {
                stringsMap = Map.insert s newLength (stringsMap store)
            }))
            return (LLVMVariable {
                type' = Str,
                address = LLVMAddressString s newLength,
                blockLabel = currentLabel store,
                ident = Nothing
            })
compileExpr (Neg expr) = compileExpr (EAdd (ELitInt 0) Minus expr)
compileExpr (Not expr) = do
    store <- get
    var <- compileExpr expr
    trueVar <- compileExpr (ELitTrue)
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = Bool,
        address = LLVMAddressRegister nextRegister,
        blockLabel = currentLabel store,
        ident = Nothing
    })
    emit (Operation trueVar Xor var result)
    return result
compileExpr (EMul expr1 mulOp expr2) = do
    store <- get
    left <- compileExpr expr1
    right <- compileExpr expr2
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = Int,
        address = LLVMAddressRegister nextRegister,
        blockLabel = currentLabel store,
        ident = Nothing
    })
    emit (Operation left (MulBinOp mulOp) right result)
    return result
compileExpr (EAdd expr1 addOp expr2) = do
    store <- get
    left <- compileExpr expr1
    right <- compileExpr expr2
    nextRegister <- getNextRegisterCounter
    case (type' left) of
        Int -> do
            let result = (LLVMVariable {
                type' = Int,
                address = LLVMAddressRegister nextRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            emit (Operation left (AddBinOp addOp) right result)
            return result
        Str -> do
            let result = (LLVMVariable {
                type' = Str,
                address = LLVMAddressRegister nextRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            emit (Call result (Ident "__concatStrings") [left, right])
            return result


compileExpr expr = error "not implemented"

getNextRegisterCounter :: GenM Integer
getNextRegisterCounter = do
    modify (\store ->
        store { 
            registerCounter = (registerCounter store) + 1
        })
    store <- get
    return $ registerCounter store

getLoc :: Ident -> GenM LLVMAddress
getLoc ident = do
    varsMap <- asks vars
    let var = fromJust $ Map.lookup ident varsMap
    return $ address var

getVar :: Ident -> GenM LLVMVariable
getVar ident = do
    varsMap <- asks vars
    return $ fromJust $ Map.lookup ident varsMap