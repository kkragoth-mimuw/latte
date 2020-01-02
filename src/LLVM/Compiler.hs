{-# LANGUAGE LambdaCase #-}

module LLVM.Compiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import AbsLatte 

type CM a = (ReaderT Env (StateT Store IO) ) a

runCompiler :: (Compilable program) => program -> IO String
runCompiler program = do
    (_, store) <- runStateT (runReaderT (compile program) initEnv) initStore
    return $ show store

class Compilable f  where
    compile :: f -> CM ()

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
    functionBlocks :: Map.Map Ident BlockMap
} deriving (Show)

initStore = Store {
    currentFunction = Ident "",
    currentLabel = 0,
    functions = Map.empty,
    labelCounter = 0,
    registerCounter = 0,
    functionBlocks = Map.empty
}

type BlockMap = Map.Map Integer LLVMBlock

-- data FunctionBlocks = FunctionBlocks {
--     -- initialFunctionBlock :: LLVMBlock,
--     blocks :: [LLVMBlock]
-- } deriving (Show)

data LLVMBlock = LLVMBlock {
    label :: Integer,
    code :: [LLVMInstruction]
} deriving (Show)


data BinOp = AddBinOp AddOp | MulBinOp MulOp | RelBinOp RelOp | AndOp | OrOp
data UnOp = NotOp

data LLVMInstruction = Branch Integer
    | MemoryStore LLVMVariable LLVMVariable

data LLVMAddress = LLVMAddressImmediate Integer
    | LLVMAddressRegister Integer 

data LLVMVariable = LLVMVariable {
    type' :: Type,
    address :: LLVMAddress,
    blockLabel :: Integer
}

instance Show LLVMInstruction where
    show (Branch labelNumber) = ""

instance Compilable Program where
    compile (Program topdefs) = do
        forM_ topdefs fillFunctionsInformation
        compileFnDefs topdefs
        return ()


fillFunctionsInformation :: TopDef -> CM ()
fillFunctionsInformation (FnDef type' ident args _) = do
    modify (\store -> store {
        functions = Map.insert ident (Fun type' (map (\(Arg t _) -> t) args)) (functions store)
    })

emit :: LLVMInstruction -> CM ()
emit instruction = do
    store <- get
    let blockLabel = currentLabel store
    emitInSpecificBlock blockLabel instruction

emitInSpecificBlock :: Integer -> LLVMInstruction -> CM ()
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

getNewLabel :: CM Integer
getNewLabel = do
    modify (\store ->
        store { 
            labelCounter = (labelCounter store) + 1,
            currentLabel = (labelCounter store) + 1
        })
    store <- get
    return $ labelCounter store

compileFnDefs :: [TopDef] -> CM String
compileFnDefs [] = return ""
compileFnDefs (x:xs) = do
    compileFnDef x
    compileFnDefs xs

compileFnDef :: TopDef -> CM String
compileFnDef (FnDef type' ident args block) = do
    modify (\store -> store {
        currentFunction = ident,
        currentLabel = 0,
        labelCounter = 0,
        functionBlocks = Map.insert ident Map.empty (functionBlocks store)
    })
    compileBlock block
    return ""

compileBlock :: Block -> CM ()
compileBlock (Block stmts) = do
    store <- get
    let previousBlockLabel = currentLabel store
    newBlockLabel <- getNewLabel

    emitInSpecificBlock previousBlockLabel (Branch newBlockLabel)
    compileStmts stmts

    return ()

compileStmts :: [Stmt] -> CM Env
compileStmts [] = ask
compileStmts (stmt:stmts) = do
    env <- compileStmt stmt
    local (const env) (compileStmts stmts)


--data Stmt
--     = Empty
--     | BStmt Block
--     | Decl Type [Item]
--     | Ass Ident Expr
--     | Incr Ident
--     | Decr Ident
--     | Ret Expr
--     | VRet
--     | Cond Expr Stmt
--     | CondElse Expr Stmt Stmt
--     | While Expr Stmt
--     | SExp Expr
--   deriving (Eq, Ord, Show, Read)
compileStmt :: Stmt -> CM Env
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
compileStmt stmt = error (show stmt ++ "not implemented")

compileDecls :: Type -> [Item] -> CM Env
compileDecls type' [] = ask
compileDecls type' (item:items) = do
    env <- compileDecl type' item
    newEnv <- local (const env) (compileDecls type' items)
    return newEnv

compileDecl :: Type -> Item -> CM Env
compileDecl type' (NoInit ident) = error "aaa"
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
compileExpr :: Expr -> CM LLVMVariable
compileExpr (EVar ident) = do
    varsMap <- asks vars
    let var = fromJust $ Map.lookup ident varsMap
    error "aa"


compileExpr (ELitInt i) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = Int,
        address = LLVMAddressImmediate i,
        blockLabel = blockLabel
    }

compileExpr expr = error "not implemented"

getNextRegisterCounter :: CM Integer
getNextRegisterCounter = do
    modify (\store ->
        store { 
            registerCounter = (registerCounter store) + 1
        })
    store <- get
    return $ registerCounter store

getLoc :: Ident -> CM LLVMAddress
getLoc ident = do
    varsMap <- asks vars
    let var = fromJust $ Map.lookup ident varsMap
    return $ address var

getVar :: Ident -> CM LLVMVariable
getVar ident = do
    varsMap <- asks vars
    return $ fromJust $ Map.lookup ident varsMap