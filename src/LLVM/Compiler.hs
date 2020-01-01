{-# LANGUAGE LambdaCase #-}

module LLVM.Compiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map as Map

import AbsLatte 

type CM a = (ReaderT Env (StateT Store IO) ) a

runCompiler :: (Compilable program) => program -> IO String
runCompiler program = do
    (_, store) <- runStateT (runReaderT (compile program) initEnv) initStore
    return $ show store

class Compilable f  where
    compile :: f -> CM ()

data Env = Env {
    returnType :: Type
}

initEnv = Env {
    returnType = Void
}

data Store = Store {
    functions :: Map.Map Ident Type,
    labelCounter :: Integer,
    counter :: Integer
} deriving (Show)

initStore = Store {
    functions = Map.empty,
    labelCounter = 0,
    counter = 0
}

data FunctionBlocks = FunctionBlocks {
    initialFunctionBlock :: LLVMBlock,
    blocks :: [LLVMBlock]
}

data LLVMBlock = LLVMBlock {
    label :: Integer,
    code :: LLVMInstruction
}


data BinOp = AddBinOp AddOp | MulBinOp MulOp | RelBinOp RelOp | AndOp | OrOp
data UnOp = NotOp

data LLVMInstruction = 
    Branch String

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

compileFnDefs :: [TopDef] -> CM String
compileFnDefs [] = return ""
compileFnDefs (x:xs) = do
    compileFnDef x
    compileFnDefs xs

compileFnDef :: TopDef -> CM String
compileFnDef (FnDef type' ident args block) = do
    modify (\store -> store { labelCounter = 0 })
    return ""