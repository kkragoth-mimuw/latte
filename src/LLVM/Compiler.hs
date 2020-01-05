{-# LANGUAGE LambdaCase #-}

-- TODO: https://buildmedia.readthedocs.org/media/pdf/mapping-high-level-constructs-to-llvm-ir/latest/mapping-high-level-constructs-to-llvm-ir.pdf
-- on mac:  /usr/local/opt/llvm/bin/llvm-as
-- export PATH=$PATH:/usr/local/opt/llvm/bin/

-- TODO:
-- TESTING
-- Tests summary 5 Jan
-- Correct tests:  16
-- Incorrect tests:  6
-- problematic tests:
-- compilation errors: ['core001.lat', 'core010.lat', 'core012.lat', 'core021.lat']
-- lli errors: ['core003.lat', 'core020.lat']

-- PHI_OPTIMIZATIONS
-- DUMMY FRONTEND OPTIMIZATIONS
-- ADVANCED FRONTEND OPTIMIZATIONS

module LLVM.Compiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Text.Printf

import AbsLatte 

debugPrint = 0
phiOptimization = 1

type GenM a = (ExceptT CompilationError (ReaderT Env (StateT Store IO) )) a

data CompilationError = CompilationErrorFunctionHasNoExplicitReturn Ident

instance Show CompilationError where
    show (CompilationErrorFunctionHasNoExplicitReturn (Ident functionIdent)) = printf "Function %s has missing return!" functionIdent

runCompiler :: (Compilable program) => program -> IO (Either String String)
runCompiler program = do
    runInfo <- runStateT (runReaderT (runExceptT (compile program)) initEnv) initStore
    case runInfo of 
        (Left compilationError, store) -> return $ Left (show compilationError)
        (Right res, store) -> do
            let predefinedFunctions = showPredefinedFunctions
            let stringsDecl =  showStringsDeclarations (stringsMap store)
            return $ Right (predefinedFunctions ++ "\n" ++stringsDecl ++ res)

class Compilable f  where
    compile :: f -> GenM String

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
    functionsLabelFollowUp :: Map.Map Ident LabelFollowUpMap,
    stringsMap :: Map.Map String Integer,
    initBlock :: Integer
} deriving (Show)

initStore = Store {
    currentFunction = Ident "",
    currentLabel = 0,
    functions = Map.fromList (predefinedFunctions),
    labelCounter = 0,
    registerCounter = -1,
    functionBlocks = Map.empty,
    functionsLabelFollowUp = Map.empty,
    stringsMap = Map.empty,
    initBlock = 0
}

type BlockMap = Map.Map Integer LLVMBlock
type LabelFollowUpMap = Map.Map Integer Integer

data LLVMBlock = LLVMBlock {
    label :: Integer,
    code :: [LLVMInstruction],
    inEdges :: [Integer],
    outEdges :: [Integer]
} deriving (Show)

showStringsDeclarations :: Map.Map String Integer -> String
showStringsDeclarations stringMap = intercalate ("\n") (
        map (\(str, i) -> printf ("@s%s = private constant [%s x i8] c\"%s\\00\"") (show i) (show $ (length str) + 1) str)
        (Map.toList stringMap)
    ) ++ "\n\n"

data Op = AddBinOp AddOp | MulBinOp MulOp | RelBinOp RelOp | AndOp | OrOp | XorOp deriving (Show)

data LLVMInstruction = Alloca LLVMVariable
    | BitcastString String Integer LLVMVariable
    | Operation LLVMVariable Op LLVMVariable LLVMVariable
    | MemoryStore LLVMVariable LLVMVariable
    | Load LLVMVariable LLVMVariable
    | ReturnVoid
    | Return LLVMVariable
    | Branch Integer
    | Call LLVMVariable Ident [LLVMVariable]
    | CallVoid Ident [LLVMVariable]
    | Phi LLVMVariable [LLVMVariable]
    | BranchConditional LLVMVariable Integer Integer deriving (Show)

data LLVMAddress = LLVMAddressVoid
    | LLVMAddressImmediate Integer
    | LLVMAddressNamedRegister String
    | LLVMAddressRegister Integer deriving (Show)

data LLVMType = LLVMType Type | LLVMTypePointer LLVMType deriving (Show)

data LLVMVariable = LLVMVariable {
    type' :: LLVMType,
    address :: LLVMAddress,
    blockLabel :: Integer,
    ident :: Maybe Ident
} deriving (Show)


instance Compilable Program where
    compile (Program topdefs) = do
        forM_ topdefs fillFunctionsInformation
        result <- compileFnDefs topdefs
        return result


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
        Just block -> let newMap = Map.insert blockLabel (LLVMBlock { label = label block, code = (code block) ++ [instruction], inEdges = [], outEdges = []}) (blockMap) in
            modify (\store -> store {
                functionBlocks = Map.insert (currentFunction store) (newMap) (functionBlocks store)
            })
        Nothing -> let newMap = Map.insert blockLabel (LLVMBlock { label = blockLabel, code = [instruction], inEdges = [], outEdges = []}) (blockMap) in
            modify (\store -> store {
                functionBlocks = Map.insert (currentFunction store) (newMap) (functionBlocks store)
            })

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
    result <- compileFnDef x
    result2 <- compileFnDefs xs
    return (result ++ result2)

compileFnDef :: TopDef -> GenM String
compileFnDef (FnDef type' ident args block) = do
    modify (\store -> store {
        currentFunction = ident,
        currentLabel = 0,
        labelCounter = 0,
        registerCounter = -1,
        functionBlocks = Map.insert ident (Map.fromList [(0, (LLVMBlock { label = 0, code = [], inEdges = [], outEdges = []} ))]) (functionBlocks store),
        functionsLabelFollowUp = Map.insert ident (Map.empty) (functionsLabelFollowUp store),
        initBlock = 1
    })

    newEnv <- prepareArgs args
 
    local (const newEnv) (compileBlock block)

    functionBlocksMap <- gets functionBlocks
    let blockMap = fromJust $ Map.lookup ident functionBlocksMap
    -- TODO
    optimizedBlockMap <- optimizeBlockMapReturn blockMap

    -- let optimizedBlockMap = blockMap -- noOptimize

    modify (\store -> (store {
        functionBlocks = Map.insert ident (optimizedBlockMap) functionBlocksMap
    }))

    -- TODO OPTIMIZE PHI BLOCK

    store <- get
    
    liftIO $ putStrLn $ "; " ++ "Function Label Skip Map" ++ (show ident)
    liftIO $ putStrLn ("; " ++ (show $ fromJust $ Map.lookup (currentFunction store) (functionsLabelFollowUp store)))
    liftIO $ putStrLn $ ""

    let functionDef = printf ("define %s @%s(%s) {\n") (showTypeInLLVM type') (showIdent ident) (showArgs args)
    let blockCode = showBlocks (Map.elems $ fromJust $ Map.lookup (ident) (functionBlocks store))

    return (functionDef ++ blockCode ++ "}\n\n")

optimizeBlockMapReturn :: BlockMap -> GenM BlockMap
optimizeBlockMapReturn blockMap = do
    let blocks = Map.elems blockMap
    optimizedBlocks <- mapM optimizeBlock blocks
    let optimizedMap = foldr (\block -> \m -> (Map.insert (label block) (block) m)) Map.empty optimizedBlocks
    return optimizedMap

optimizeBlock :: LLVMBlock -> GenM LLVMBlock
optimizeBlock block = do
    store <- get

    let newCode = codeUntilBranchOrReturn (code block)

    if (length newCode == 0) then do
        throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
    else do
        case (last newCode) of
            ReturnVoid -> return $ block { code = newCode }
            Return _ -> return $ block { code = newCode }
            Branch _ -> return $ block { code = newCode }
            BranchConditional _ _ _ -> return $ block { code = newCode }
            _ -> throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
        

codeUntilBranchOrReturn :: [LLVMInstruction] -> [LLVMInstruction]
codeUntilBranchOrReturn [] = []
codeUntilBranchOrReturn (x:xs) = case x of
    ReturnVoid -> [x]
    Return _ -> [x]
    Branch _ -> [x]
    BranchConditional _ _ _ -> [x]
    _ -> [x] ++ codeUntilBranchOrReturn xs

optimizeAndCheckLLVMBlocks :: [LLVMBlock] -> GenM ()
optimizeAndCheckLLVMBlocks blocks = error "no blocks" 

prepareArgs :: [Arg] -> GenM Env
prepareArgs [] = ask
prepareArgs (x:xs) = do
    env <- prepareArg x
    newEnv <- local (const env) (prepareArgs xs)
    return newEnv

prepareArg :: Arg -> GenM Env
prepareArg (Arg type' ident@(Ident argName)) = do
    store <- get
    env <- ask
    newRegister <- getNextRegisterCounter
    let allocaVar = (LLVMVariable {
        type' = LLVMTypePointer (LLVMType type'),
        address = LLVMAddressRegister newRegister,
        blockLabel = (currentLabel store),
        ident = Just ident
    })
    let funcArg = (LLVMVariable {
        type' = LLVMType type',
        address = LLVMAddressNamedRegister argName,
        blockLabel = (currentLabel store),
        ident = Nothing
    })
    emit (Alloca allocaVar)
    emit (MemoryStore funcArg allocaVar)
    return $ env { vars = Map.insert ident allocaVar (vars env)}


compileBlock :: Block -> GenM ()
compileBlock (Block []) = return ()
compileBlock (Block [Empty]) = return ()
compileBlock (Block stmts) = do
    store <- get
    let isInitBlock = initBlock store

    modify (\store -> store { initBlock = 1 })

    let currentBlock = fromJust $ Map.lookup (currentLabel store) (fromJust $ Map.lookup (currentFunction store) (functionBlocks store))
    let codeLen = length (code currentBlock)

    if (codeLen == 0 || isInitBlock == 1) then
        compileStmts stmts
    else do
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
    case stmt of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmt
    afterIfBlock <- getNewLabel

    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp  (BranchConditional condition ifTrueStmtsLabel afterIfBlock)

    emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)

    ask
compileStmt (CondElse expr stmtTrue stmtFalse) = do
    condition <- compileExpr expr
    previousLabel <- gets currentLabel
    ifTrueStmtsLabel <- getNewLabel
    case stmtTrue of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmtTrue
    ifFalseStmtsLabel <- getNewLabel
    case stmtFalse of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmtFalse
    afterIfBlock <- getNewLabel

    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp (BranchConditional condition ifTrueStmtsLabel ifFalseStmtsLabel)
    emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)
    emitInSpecificBlock ifFalseStmtsLabel (Branch afterIfBlock)
    ask
compileStmt (While expr stmt) = do
    preConditionLabel <- gets currentLabel
    conditionLabel <- getNewLabel
    emitInSpecificBlock preConditionLabel (Branch conditionLabel)
    condition <- compileExpr expr
    ifTrueBodyLabel <- getNewLabel
    case stmt of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmt
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
        type' = LLVMTypePointer (LLVMType type'),
        address = LLVMAddressRegister variableRegister,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Alloca allocaVar)
    emit (MemoryStore rhs allocaVar)
    return $ env { vars = Map.insert ident allocaVar (vars env)}
compileDecl type' (Init ident expr) = do
    rhs <- compileExpr expr
    variableRegister <- getNextRegisterCounter
    env <- ask
    store <- get
    let allocaVar = (LLVMVariable {
        type' = LLVMTypePointer (LLVMType type'),
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
        type' = LLVMType Int,
        address = LLVMAddressImmediate 0,
        blockLabel = currentLabel store,
        ident = Nothing
    })
defaultVariable Bool = do
    store <- get
    return $ (LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressImmediate 0,
        blockLabel = currentLabel store,
        ident = Nothing
    })
defaultVariable Str = error "defaultStr not implemented"

compileExpr :: Expr -> GenM LLVMVariable
compileExpr (EVar ident) = do
    store <- get
    varsMap <- asks vars
    let var = fromJust $ Map.lookup ident varsMap
    newRegisterNumber <- getNextRegisterCounter
    let registerVar = (LLVMVariable {
        type' = dereferencePointer $ type' var,
        address = LLVMAddressRegister newRegisterNumber,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Load registerVar var)
    return registerVar
compileExpr (ELitInt i) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = LLVMType Int,
        address = LLVMAddressImmediate i,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr(ELitTrue) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressImmediate 1,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr(ELitFalse) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = LLVMType Bool,
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
                type' = LLVMType Void,
                address = LLVMAddressVoid,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
        (Fun type' _) -> do
            newRegister <- getNextRegisterCounter
            let result = (LLVMVariable {
                type' = LLVMType type',
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
            newRegister <- getNextRegisterCounter

            let result = (LLVMVariable {
                type' = LLVMType Str,
                address = LLVMAddressRegister newRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            
            emit (BitcastString s i result)

            return result

        Nothing -> do
            let newLength = toInteger $ (Map.size (stringsMap store)) + 1
            modify (\store -> (store {
                stringsMap = Map.insert s newLength (stringsMap store)
            }))

            newRegister <- getNextRegisterCounter

            let result = (LLVMVariable {
                type' = LLVMType Str,
                address = LLVMAddressRegister newRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            
            emit (BitcastString s newLength result)

            return result
            
compileExpr (Neg expr) = compileExpr (EAdd (ELitInt 0) Minus expr)
compileExpr (Not expr) = do
    store <- get
    var <- compileExpr expr
    trueVar <- compileExpr (ELitTrue)
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressRegister nextRegister,
        blockLabel = currentLabel store,
        ident = Nothing
    })
    emit (Operation trueVar XorOp var result)
    return result
compileExpr (EMul expr1 mulOp expr2) = do
    store <- get
    left <- compileExpr expr1
    right <- compileExpr expr2
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMType Int,
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
        LLVMType Int -> do
            let result = (LLVMVariable {
                type' = LLVMType Int,
                address = LLVMAddressRegister nextRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            emit (Operation left (AddBinOp addOp) right result)
            return result
        LLVMType Str -> do
            let result = (LLVMVariable {
                type' = LLVMType Str,
                address = LLVMAddressRegister nextRegister,
                blockLabel = currentLabel store,
                ident = Nothing
            })
            emit (Call result (Ident "__concatStrings") [left, right])
            return result
compileExpr (ERel expr1 relOp expr2) = do
    store <- get
    left <- compileExpr expr1
    right <- compileExpr expr2
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressRegister nextRegister,
        blockLabel = currentLabel store,
        ident = Nothing
    })
    emit (Operation left (RelBinOp relOp) right result)
    return result
-- compileExpr (EAnd expr1 expr2) = do
--     store <- get
--     left <- compileExpr expr1
--     right <- compileExpr expr2
--     nextRegister <- getNextRegisterCounter
--     let result = (LLVMVariable {
--         type' = LLVMType Bool,
--         address = LLVMAddressRegister nextRegister,
--         blockLabel = currentLabel store,
--         ident = Nothing
--     })
--     emit (Operation left (AndOp) right result)
--     return result
-- compileExpr (EOr expr1 expr2) = do
--     store <- get
--     left <- compileExpr expr1
--     right <- compileExpr expr2
--     nextRegister <- getNextRegisterCounter
--     let result = (LLVMVariable {
--         type' = LLVMType Bool,
--         address = LLVMAddressRegister nextRegister,
--         blockLabel = currentLabel store,
--         ident = Nothing
--     })
--     emit (Operation left (OrOp) right result)
--     return result

-- from slides
-- genCond (CAnd c1 c2) lTrue lFalse = do
--     lMid <- freshLabel
--     genCond c1 lMid lFalse
--     emit $ placeLabel lMid
--     genCond c2 lTrue lFalse
-- genCond (COr c1 c2) lTrue lFalse = do
--     lMid <- freshLabel
--     genCond c1 lTrue lMid
--     emit $ placeLabel lMid
--     genCond c2 lTrue lFalse
compileExpr (EAnd expr1 expr2) = do
    store <- get
    let previousLabel = currentLabel store
    left <- compileExpr expr1

    middleLabel <- getNewLabel
    right <- compileExpr expr2

    endLabel <- getNewLabel

    emitInSpecificBlock previousLabel (BranchConditional left middleLabel endLabel)
    middleLabelFollowUp <- getLabelFollowUp middleLabel
    emitInSpecificBlock middleLabelFollowUp (Branch endLabel)

    blockLabel <- gets currentLabel
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressRegister nextRegister,
        blockLabel = blockLabel,
        ident = Nothing
    })

    emit (Phi result [left, right])

    -- FOLLOW UP
    store1 <- get
    let labelFollowUpMap = fromJust $ Map.lookup (currentFunction store1) (functionsLabelFollowUp store1)
    let updatedFollowUpMap = Map.insert previousLabel endLabel labelFollowUpMap
    modify (\store -> (store {
        functionsLabelFollowUp = Map.insert (currentFunction store) (updatedFollowUpMap) (functionsLabelFollowUp store)
    }))

    return result

compileExpr (EOr expr1 expr2) = do
    store <- get
    let previousLabel = currentLabel store
    left <- compileExpr expr1

    middleLabel <- getNewLabel
    right <- compileExpr expr2

    endLabel <- getNewLabel

    emitInSpecificBlock previousLabel (BranchConditional left endLabel middleLabel)
    
    middleLabelFollowUp <- getLabelFollowUp middleLabel
    emitInSpecificBlock middleLabelFollowUp (Branch endLabel)

    blockLabel <- gets currentLabel
    nextRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMType Bool,
        address = LLVMAddressRegister nextRegister,
        blockLabel = blockLabel,
        ident = Nothing
    })

    emit (Phi result [left, right])

        -- FOLLOW UP
    store1 <- get
    let labelFollowUpMap = fromJust $ Map.lookup (currentFunction store1) (functionsLabelFollowUp store1)
    let updatedFollowUpMap = Map.insert previousLabel endLabel labelFollowUpMap
    modify (\store -> (store {
        functionsLabelFollowUp = Map.insert (currentFunction store) (updatedFollowUpMap) (functionsLabelFollowUp store)
    }))

    return result

getLabelFollowUp :: Integer -> GenM Integer
getLabelFollowUp label = do
    store <- get
    let labelFollowUpMap = fromJust $ (Map.lookup (currentFunction store) (functionsLabelFollowUp store))
    case Map.lookup label labelFollowUpMap of
        Just i -> getLabelFollowUp i
        Nothing -> return label
        
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

showIdent :: Ident -> String
showIdent (Ident s) = s

showArgs :: [Arg] -> String
showArgs args = intercalate (", ") (map showArg args)

showArg :: Arg -> String
showArg (Arg type' ident) = printf ("%s %%%s") (showTypeInLLVM type') (showIdent ident)

showBlocks :: [LLVMBlock] -> String
showBlocks [] = ""
showBlocks (x:xs) = if (debugPrint == 0) then (showBlock x ++ showBlocks xs) else (showBlockDebug x ++ showBlocks xs)

showBlock :: LLVMBlock -> String
showBlock block = printf("L%s:\n\t%s\n") (show $ label block) (intercalate "\n\t" (map printLLVMInstruction (code block)))

showBlockDebug :: LLVMBlock -> String
showBlockDebug block = printf("L%s:\n\t%s\n") (show $ label block) (intercalate "\n\t" (map show (code block)))

showTypeInLLVM :: Type -> String
showTypeInLLVM Void = "void"
showTypeInLLVM Int = "i32"
showTypeInLLVM Str = "i8*"
showTypeInLLVM Bool = "i1"
showTypeInLLVM _ = ""

dereferencePointer :: LLVMType -> LLVMType
dereferencePointer (LLVMTypePointer t) = t
dereferencePointer _ = error "not a pointer!"

-- I dont want to do this in typeclass SHOW cause I need additional info for debuging for optimizations
printLLVMType :: LLVMType -> String
printLLVMType (LLVMType type') = showTypeInLLVM type'
printLLVMType (LLVMTypePointer t) = (printLLVMType t) ++ "*"

printLLVMVarType :: LLVMVariable -> String
printLLVMVarType = printLLVMType . type'

printLLVMVarLabel :: LLVMVariable -> String
printLLVMVarLabel var = "L" ++ (show $ blockLabel var)

printLLVMAddress :: LLVMAddress -> String
printLLVMAddress LLVMAddressVoid = ""
printLLVMAddress (LLVMAddressImmediate integer) = show integer
printLLVMAddress (LLVMAddressRegister label) = printf("%%r%s") (show label)
printLLVMAddress (LLVMAddressNamedRegister strLabel) = printf("%%%s") (strLabel)

printLLVMVarAddress :: LLVMVariable -> String
printLLVMVarAddress = printLLVMAddress . address

printLLVMInstruction :: LLVMInstruction -> String
printLLVMInstruction ReturnVoid = "ret void"
printLLVMInstruction (Alloca llvmVariable) = printf "%s = alloca %s" (printLLVMVarAddress llvmVariable) (printLLVMType $ dereferencePointer $ type' llvmVariable)
printLLVMInstruction (BitcastString s i var) = printf "%s = bitcast [%s x i8]* @s%s to i8*" (printLLVMVarAddress var) (show $ (length s) + 1) (show i)
printLLVMInstruction (Return llvmVariable) = printf ("ret %s %s") (printLLVMVarType llvmVariable) (printLLVMVarAddress llvmVariable)
printLLVMInstruction (Branch label) = printf ("br label %%L%s") (show label)
printLLVMInstruction (BranchConditional v l1 l2) = printf ("br %s %s, label %%L%s, label %%L%s") (printLLVMVarType v) (printLLVMVarAddress v) (show l1) (show l2)
printLLVMInstruction (MemoryStore s t) = printf ("store %s %s, %s %s") (printLLVMVarType s) (printLLVMVarAddress s) (printLLVMVarType t) (printLLVMVarAddress t)
printLLVMInstruction (Load r v) = printf ("%s = load %s, %s %s") (printLLVMVarAddress r) (printLLVMVarType r) (printLLVMVarType v) (printLLVMVarAddress v)
printLLVMInstruction (Call v (Ident fnName) args) = printf ("%s = call %s @%s(%s)") (printLLVMVarAddress v) (printLLVMVarType v) fnName (intercalate "," (zipWith (\t -> \r -> (printf "%s %s") t r) (map printLLVMVarType args) (map printLLVMVarAddress args)))
printLLVMInstruction (CallVoid (Ident fnName) args) = printf ("call void @%s(%s)") fnName (intercalate "," (zipWith (\t -> \r -> (printf "%s %s") t r) (map printLLVMVarType args) (map printLLVMVarAddress args)))
printLLVMInstruction (Operation l op r result) = case op of
    (AddBinOp Plus) -> printf ("%s = add %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (AddBinOp Minus) -> printf ("%s = sub %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (AndOp) -> printf ("%s = and %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (OrOp) -> printf ("%s = or %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (XorOp) -> printf ("%s = xor %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (RelBinOp relOp) -> case relOp of
        (LTH) -> printf ("%s = icmp slt %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
        (LE) -> printf ("%s = icmp sle %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
        (GTH) -> printf ("%s = icmp sgt %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
        (GE) -> printf ("%s = icmp sge %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
        (NE) -> printf ("%s = icmp ne %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
        (EQU) -> printf ("%s = icmp eq %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType l) (printLLVMVarAddress l) (printLLVMVarAddress r)
printLLVMInstruction (Phi v vars) = printf ("%s = phi %s %s") (printLLVMVarAddress v) (printLLVMVarType v) (printPhiVars vars)

printPhiVars :: [LLVMVariable] -> String
printPhiVars vars = intercalate (", ") (map (\var -> (printf ("[ %s, %%%s ]") (printLLVMVarAddress var) (printLLVMVarLabel var))) vars)

predefinedFunctions :: [(Ident, Type)]
predefinedFunctions = ([
        (Ident "printInt", (Fun Void [Int])),
        (Ident "printString", (Fun Void [Str])),
        (Ident "error", (Fun Void [])),
        (Ident "readInt", (Fun Int [])),
        (Ident "readString", (Fun Str [])),
        (Ident "__concatStrings", (Fun Str [Str, Str]))
    ])

showPredefinedFunctions :: String
showPredefinedFunctions = (intercalate ("\n") ([
        "declare void @printInt(i32)",
        "declare void @printString(i8*)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()",
        "declare i8* @__concatStrings(i8*, i8*)"
    ])) ++ "\n"