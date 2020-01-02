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
    envs :: Map.Map String String  
}

initEnv = Env {
}

data Store = Store {
    currentFunction :: Ident,
    currentLabel :: Integer,
    functions :: Map.Map Ident Type,
    labelCounter :: Integer,
    counter :: Integer,
    functionBlocks :: Map.Map Ident BlockMap
} deriving (Show)

initStore = Store {
    functions = Map.empty,
    labelCounter = 0,
    counter = 0
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

data LLVMInstruction = 
    Branch Integer

instance Show LLVMInstruction where
    show (Branch labelNumber) = ""

instance Compilable Program where
    compile (Program topdefs) = do
        forM_ topdefs fillFunctionsInformation

        stat <- get
        liftIO $ putStrLn $ show stat


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

createNewBlockLabel :: CM Integer
createNewBlockLabel = do
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
        labelCounter = 0
    })
    compileBlock block
    return ""

compileBlock :: Block -> CM ()
compileBlock (Block stmts) = do
    store <- get
    let previousBlock = currentLabel store

    newBlockLabel <- createNewBlockLabel

    return

compileStmts :: [Stmt] -> CM Env
compileStmts [] = ask
compileStmts (stmt:stmts) = do
    env <- compileStmt stmt
    local (const env) (compileStmts stmts)

compileStmt :: Stmt -> CM Env
compileStmt (Empty) = ask
compileStmt (BStmt block) = do
    compileBlock block
    ask

