{-# LANGUAGE LambdaCase #-}

-- TODO: https://buildmedia.readthedocs.org/media/pdf/mapping-high-level-constructs-to-llvm-ir/latest/mapping-high-level-constructs-to-llvm-ir.pdf
-- export PATH=$PATH:/usr/local/opt/llvm/bin/

-- TODO:
-- Class Typechecker
-- Testing, testing, testing...

module LLVMCompiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate, findIndex, find)
import Text.Printf

import AbsLatte 

import Utils

debugPrint = 0

thisIdent = (Ident "self") 
shouldCutOff = True

type GenM a = (ExceptT CompilationError (ReaderT Env (StateT Store IO) )) a

data CompilationError = CompilationErrorFunctionHasNoExplicitReturn Ident | CompilationError String

instance Show CompilationError where
    show (CompilationErrorFunctionHasNoExplicitReturn (Ident functionIdent)) = printf "Function %s has missing return!" functionIdent
    show (CompilationError str) = str

runLLVMCompiler :: (Compilable program) => program -> IO (Either String String)
runLLVMCompiler program = do
    runInfo <- runStateT (runReaderT (runExceptT (compile program)) initEnv) initStore
    case runInfo of 
        (Left compilationError, store) -> return $ Left (show compilationError)
        (Right res, store) -> do
            let predefinedFunctions = showPredefinedFunctions
            let stringsDecl =  showStringsDeclarations (stringsMap store)
            let classDecl =  showClassesDeclarations (classes store)
            
            return $ Right (predefinedFunctions ++ "\n" ++stringsDecl ++ "\n" ++ classDecl ++ "\n" ++ res)

class Compilable f  where
    compile :: f -> GenM String

data Env = Env {
    vars :: Map.Map Ident LLVMVariable,
    afterBlockJump :: Maybe Integer,
    currentClass :: Maybe LLVMClass
}

initEnv = Env {
    vars = Map.empty,
    afterBlockJump = Nothing,
    currentClass = Nothing
}

data Store = Store {
    currentFunction :: Ident,
    currentLabel :: Integer,
    functions :: Map.Map Ident Type,
    classes :: Map.Map Ident LLVMClass,
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
    classes = Map.empty,
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
        map (\(str, i) -> printf ("@s%s = private constant [%s x i8] c\"%s\\00\"") (show i) (show $ (length str) + 1) (escapeString str))
        (Map.toList stringMap)
    ) ++ "\n\n"

escapeString :: String -> String
escapeString [] = ""
escapeString ('\\':xs) = "\\5C" ++ escapeString xs
escapeString ('\"':xs) = "\\22" ++ escapeString xs
escapeString ('\n':xs) = "\\0A" ++ escapeString xs
escapeString ('\t':xs) = "\\09" ++ escapeString xs
escapeString (x:xs) = [x] ++ escapeString xs
-- @.str = private unnamed_addr constant [9 x i8] c"\5Ca\5Cn\0A\09b\22\00", align 1

showClassesDeclarations :: Map.Map Ident LLVMClass -> String
showClassesDeclarations classMap = intercalate ("\n") (
        map printLLVMClass (Map.elems classMap)
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
    | Malloc Integer Integer
    | BitcastMalloc Integer Integer Type
    | Bitcast LLVMVariable LLVMType LLVMVariable LLVMType
    | GEPClass LLVMVariable Type LLVMVariable Integer
    
    | BranchConditional LLVMVariable Integer Integer deriving (Show)

data LLVMAddress = LLVMAddressVoid
    | LLVMAddressNull
    | LLVMAddressImmediate Integer
    | LLVMAddressNamedRegister String
    | LLVMAddressRegister Integer deriving (Show)

data LLVMType = LLVMType Type | LLVMTypePointer LLVMType deriving (Show, Eq)

data LLVMVariable = LLVMVariable {
    type' :: LLVMType,
    address :: LLVMAddress,
    blockLabel :: Integer,
    ident :: Maybe Ident
} deriving (Show)

data LLVMClass = LLVMClass {
    llvmBaseClassName :: Maybe Ident,
    llvmClassName :: Ident,
    llvmClassFields :: [LLVMClassField],
    llvmClassMethods :: [(Ident, ClassPole)]
} deriving (Show)

data LLVMClassField = LLVMClassField {
    classFieldName :: Ident,
    classFieldType :: LLVMType
} deriving (Show)

classToLLVMClass :: Class -> LLVMClass
classToLLVMClass c = LLVMClass {
    llvmBaseClassName = baseClassName c,
    llvmClassName = className c,
    llvmClassMethods = classMethods c,
    llvmClassFields = map (\(ClassFieldDef type' ident') -> case type' of
                                                                        c@(ClassType _) -> LLVMClassField {
                                                                                                classFieldName = ident',
                                                                                                classFieldType = LLVMTypePointer (LLVMType c)
                                                                                            }
                                                                        _ -> LLVMClassField {
                                                                                    classFieldName = ident',
                                                                                    classFieldType = LLVMType type'
                                                                                }
        ) (classFields c)
}

calculateClassSize :: LLVMClass -> Integer
calculateClassSize c = sum fieldSizes where
                                fieldSizes = map calculateFieldSize (llvmClassFields c)

calculateFieldSize :: LLVMClassField -> Integer
calculateFieldSize cf = calculateTypeSize (classFieldType cf)

calculateTypeSize :: LLVMType -> Integer
calculateTypeSize (LLVMTypePointer _) = 8
calculateTypeSize (LLVMType Str) = 8
calculateTypeSize (LLVMType Int) = 4
calculateTypeSize (LLVMType Boolean) = 1 -- or 4??!?!?!? TODO

instance Compilable Program where
    compile (Program topdefs) = do
        forM_ topdefs fillTopDefInformation
        let classesMap = Map.map classToLLVMClass (createClassMapFromTopDefs topdefs) 
        modify (\store -> store {
            classes = classesMap
        })
        forM (Map.elems classesMap) fillClassMethodsInformation
        methodsDecl <- compileMethods
        result <- compileFnDefs topdefs
        return $ methodsDecl ++ "\n" ++ result


fillTopDefInformation :: TopDef -> GenM ()
fillTopDefInformation (FnDef type' ident args _) = do
    modify (\store -> store {
        functions = Map.insert ident (Fun type' (map (\(Arg t _) -> t) args)) (functions store)
    })
fillTopDefInformation _ = return ()

fillClassMethodsInformation :: LLVMClass -> GenM ()
fillClassMethodsInformation llvmClass = do
    forM_ (llvmClassMethods llvmClass) (fillMethodInformation llvmClass)

fillMethodInformation :: LLVMClass -> (Ident, ClassPole) -> GenM ()
fillMethodInformation c (methodClass, m@(ClassMethodDef _ _ _ _)) = do
    if ((llvmClassName c) == methodClass) then do
        let (FnDef type' ident args _) = methodToFunctionConverter c m
        modify (\store -> store {
            functions = Map.insert ident (Fun type' (map (\(Arg t _) -> t) args)) (functions store)
        })
    else return ()
fillMethodInformation _ _ = return ()

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

setAsCurrentLabel :: Integer -> GenM ()
setAsCurrentLabel label = do
    modify (\store -> store {
        currentLabel = label
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

compileMethods :: GenM String
compileMethods = do
    classesM <- gets classes
    let classesList = Map.elems classesM
    result <- mapM (\c -> compileMethodDefs c (llvmClassMethods c)) classesList
    return $ intercalate ("\n") result

compileMethodDefs :: LLVMClass -> [(Ident, ClassPole)] -> GenM String
compileMethodDefs c [] = return ""
compileMethodDefs c (x:xs) = do
    result <- compileMethodDef c x
    result2 <- compileMethodDefs c xs
    return (result ++ result2)

compileMethodDef :: LLVMClass -> (Ident, ClassPole) -> GenM String
compileMethodDef c (cIdent, classMethod) = do
    if (llvmClassName c == cIdent) then do
        let fnDef = methodToFunctionConverter c classMethod

        local (\env -> env { currentClass = Just c} ) (compileFnDef (fnDef))
    else return ""

methodToFunctionConverter :: LLVMClass -> ClassPole -> TopDef
methodToFunctionConverter c (ClassMethodDef returnType (Ident methodName) args block) =
    let cNameIdent@(Ident cName) = llvmClassName c in
    let mangledMethodName = (Ident (cName ++ "__" ++ methodName)) in
    let thisArg = Arg (ClassType cNameIdent) thisIdent in
    let mangledArgs = [thisArg] ++ args in
    (FnDef returnType mangledMethodName mangledArgs block)

compileFnDefs :: [TopDef] -> GenM String
compileFnDefs [] = return ""
compileFnDefs (x:xs) = do
    result <- compileFnDef x
    result2 <- compileFnDefs xs
    return (result ++ result2)

compileFnDef :: TopDef -> GenM String
compileFnDef (FnDef type' ident args (Block stmts)) = do
    modify (\store -> store {
        currentFunction = ident,
        currentLabel = 0,
        labelCounter = 0,
        registerCounter = -1,
        functionBlocks = Map.insert ident (Map.fromList [(0, (LLVMBlock { label = 0, code = [], inEdges = [], outEdges = []} ))]) (functionBlocks store),
        functionsLabelFollowUp = Map.insert ident (Map.empty) (functionsLabelFollowUp store),
        initBlock = 1
    })

    let stmts' = case type' of
            Void -> stmts ++ [VRet]
            _ -> stmts

    let block = Block stmts'

    newEnv <- prepareArgs args

    local (const newEnv) (compileBlock block)

    functionBlocksMap <- gets functionBlocks
    let blockMap = fromJust $ Map.lookup ident functionBlocksMap
    usedBlockMap <- case shouldCutOff of
                    True -> optimizeBlockMapReturn blockMap
                    False -> return blockMap

    modify (\store -> (store {
        functionBlocks = Map.insert ident (usedBlockMap) functionBlocksMap
    }))

    store <- get

    let returnType = case type' of
                        (ClassType ident) -> LLVMTypePointer (LLVMType type')
                        _ -> (LLVMType type')
    
    let functionDef = printf ("define %s @%s(%s) {\n") (printLLVMType returnType) (showIdent ident) (showArgs args)
    let blockCode = showBlocks (Map.elems $ fromJust $ Map.lookup (ident) (functionBlocks store))

    return (functionDef ++ blockCode ++ "}\n\n")

compileFnDef _ = return ""

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
            Branch l -> do
                case Map.lookup l (fromJust $ Map.lookup (currentFunction store) (functionBlocks store)) of
                    Nothing -> throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
                    Just _ -> return $ block { code = newCode }
            BranchConditional _ l r -> do
                let lLabelBlock = Map.lookup l (fromJust $ Map.lookup (currentFunction store) (functionBlocks store))
                let rLabelBlock = Map.lookup r (fromJust $ Map.lookup (currentFunction store) (functionBlocks store))
                case (lLabelBlock, rLabelBlock) of
                    (Nothing, _) -> throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
                    (_, Nothing) -> throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
                    _ -> return $ block { code = newCode }
            _ -> throwError $ CompilationErrorFunctionHasNoExplicitReturn (currentFunction store)
        

codeUntilBranchOrReturn :: [LLVMInstruction] -> [LLVMInstruction]
codeUntilBranchOrReturn [] = []
codeUntilBranchOrReturn (x:xs) = case x of
    ReturnVoid -> [x]
    Return _ -> [x]
    Branch _ -> [x]
    BranchConditional _ _ _ -> [x]
    _ -> [x] ++ codeUntilBranchOrReturn xs


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

    let resultType = case type' of
                        (ClassType _) -> LLVMTypePointer (LLVMType type')
                        _ -> LLVMType type'
                        
    newRegister <- getNextRegisterCounter
    let allocaVar = (LLVMVariable {
        type' = LLVMTypePointer resultType,
        address = LLVMAddressRegister newRegister,
        blockLabel = (currentLabel store),
        ident = Just ident
    })
    let funcArg = (LLVMVariable {
        type' = resultType,
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
compileStmts [] = do
    env <- ask
    case (afterBlockJump env) of
        Nothing -> return env
        Just c -> do 
            emit (Branch c)
            return env
compileStmts (stmt:stmts) = do
    case stmts of 
        [] -> case stmt of
            c@(Cond _ _) -> compileStmtWithoutBranchToNextBlock c
            c@(CondElse _ _ _) -> compileStmtWithoutBranchToNextBlock c
            w@(While x _) | x /= ELitTrue-> compileStmtWithoutBranchToNextBlock w
            _ -> compileStmt stmt
        _ -> do
            env <- compileStmt stmt
            local (const env) (compileStmts stmts)

compileStmt :: Stmt -> GenM Env
compileStmt (Empty) = ask
compileStmt (BStmt block) = do
    compileBlock block
    ask
compileStmt (Decl type' items) = compileDecls type' items
compileStmt (Ass lvalue expr) = do
    lvalueMaybe <- getLValue lvalue
    lhs <- case (lvalueMaybe) of
                    c@(Just lhs) -> return lhs
                    Nothing -> do
                        lvalueMaybe' <- getLValue (addThisToLValue lvalue)
                        return $ fromJust $ lvalueMaybe'
    rhs <- compileExpr expr

    case (type' rhs, dereferencePointer $ type' lhs) of
        (a, b) | a == b -> do
            emit (MemoryStore rhs lhs)
        _ -> do
            newRegister <- getNextRegisterCounter
            blockLabel <- gets currentLabel

            let typeCastResult = (LLVMVariable {
                type' = dereferencePointer $ type' lhs,
                address = LLVMAddressRegister newRegister,
                blockLabel = blockLabel,
                ident = Nothing
            })

            emit $ Bitcast typeCastResult (type' rhs) rhs (dereferencePointer $ type' lhs)
            emit $ MemoryStore (typeCastResult) lhs
        

    ask
compileStmt (Incr lvalue) = compileStmt (Ass lvalue (EAdd (ELValue lvalue) Plus (ELitInt 1)))
compileStmt (Decr lvalue) = compileStmt (Ass lvalue (EAdd (ELValue lvalue) Minus (ELitInt 1)))
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
    env <- ask
    condition <- compileExpr expr
    previousLabel <- gets currentLabel
    ifTrueStmtsLabel <- getNewLabel

    afterIfBlock <- getNewLabel
    let newEnv = env { afterBlockJump = Just afterIfBlock }
    setAsCurrentLabel ifTrueStmtsLabel

    case stmt of
        (BStmt (Block stmts)) -> do
            local (const newEnv) (compileStmts stmts)
        _ -> do
            local (const newEnv) (compileStmt stmt)
    emit (Branch afterIfBlock)

    setAsCurrentLabel afterIfBlock

    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp  (BranchConditional condition ifTrueStmtsLabel afterIfBlock)

    -- emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)

    ask
compileStmt (CondElse expr stmtTrue stmtFalse) = do
    env <- ask
    condition <- compileExpr expr
    previousLabel <- gets currentLabel

    ifTrueStmtsLabel <- getNewLabel
    ifFalseStmtsLabel <- getNewLabel
    afterIfBlock <- getNewLabel
    let newEnv = env { afterBlockJump = Just afterIfBlock }
    setAsCurrentLabel ifTrueStmtsLabel

    case stmtTrue of
        (BStmt (Block stmts)) -> do
            local (const newEnv) (compileStmts stmts)
        _ -> do
            local (const newEnv) (compileStmt stmtTrue)
    emit (Branch afterIfBlock)

    setAsCurrentLabel ifFalseStmtsLabel
    case stmtFalse of
        (BStmt (Block stmts)) -> do
            local (const newEnv) (compileStmts stmts)
        _ -> do
            local (const newEnv) (compileStmt stmtFalse)
    emit (Branch afterIfBlock)
            
    setAsCurrentLabel afterIfBlock
    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp (BranchConditional condition ifTrueStmtsLabel ifFalseStmtsLabel)
    -- emitInSpecificBlock ifTrueStmtsLabel (Branch afterIfBlock)
    -- emitInSpecificBlock ifFalseStmtsLabel (Branch afterIfBlock)
    ask
compileStmt (While ELitTrue stmt) = do
    -- good catch ! 
    preConditionLabel <- gets currentLabel
    ifTrueBodyLabel <- getNewLabel
    env <- ask
    let newEnv = env { afterBlockJump = Just ifTrueBodyLabel }
    case stmt of
        (BStmt (Block stmts)) -> do
            local (const newEnv) (compileStmts stmts)
        _ -> do
            local (const newEnv) (compileStmt stmt)
    emit (Branch ifTrueBodyLabel)

    afterWhileLabel <- getNewLabel
    emitInSpecificBlock preConditionLabel (Branch ifTrueBodyLabel)
    ask
compileStmt (While expr stmt) = do
    env <- ask
    preConditionLabel <- gets currentLabel
    conditionLabel <- getNewLabel
    emitInSpecificBlock preConditionLabel (Branch conditionLabel)
    condition <- compileExpr expr
    ifTrueBodyLabel <- getNewLabel
    afterWhileLabel <- getNewLabel
    let newEnv = env { afterBlockJump = Just afterWhileLabel}
    setAsCurrentLabel ifTrueBodyLabel 

    case stmt of
        (BStmt (Block stmts)) -> do
            local (const newEnv) (compileStmts stmts)
        _ -> do
            local (const newEnv) (compileStmt stmt)
    emit (Branch conditionLabel)

    setAsCurrentLabel afterWhileLabel
    
    conditionLabelFollowUp <- getLabelFollowUp conditionLabel
    emitInSpecificBlock conditionLabelFollowUp (BranchConditional condition ifTrueBodyLabel afterWhileLabel)
    ask

compileStmtWithoutBranchToNextBlock :: Stmt -> GenM Env
compileStmtWithoutBranchToNextBlock (Cond expr stmt) = do
    env <- ask
    condition <- compileExpr expr
    previousLabel <- gets currentLabel
    ifTrueStmtsLabel <- getNewLabel

    case stmt of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmt

    let nextBlock = fromJust $ afterBlockJump env
    emit (Branch nextBlock)

    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp  (BranchConditional condition ifTrueStmtsLabel nextBlock)

    ask

compileStmtWithoutBranchToNextBlock (CondElse expr stmtTrue stmtFalse) = do
    env <- ask
    condition <- compileExpr expr
    previousLabel <- gets currentLabel

    ifTrueStmtsLabel <- getNewLabel
    ifFalseStmtsLabel <- getNewLabel

    -- let nextBlock = fromJust $ afterBlockJump env
    setAsCurrentLabel ifTrueStmtsLabel

    case stmtTrue of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmtTrue
    
    case (afterBlockJump env) of
        Just nextBlock -> emit $ (Branch nextBlock)
        Nothing -> return ()

    setAsCurrentLabel ifFalseStmtsLabel
    case stmtFalse of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmtFalse
    case (afterBlockJump env) of
        Just nextBlock -> emit $ (Branch nextBlock)
        Nothing -> return ()
            
    previousLabelFollowUp <- getLabelFollowUp previousLabel
    emitInSpecificBlock previousLabelFollowUp (BranchConditional condition ifTrueStmtsLabel ifFalseStmtsLabel)
    ask
compileStmtWithoutBranchToNextBlock (While expr stmt) = do
    env <- ask
    
    nextBlock <- case (afterBlockJump env) of
        Just nextBlock -> return nextBlock
        Nothing -> throwError $ CompilationError "Probably missing return after while"

    preConditionLabel <- gets currentLabel
    conditionLabel <- getNewLabel
    emitInSpecificBlock preConditionLabel (Branch conditionLabel)
    condition <- compileExpr expr
    ifTrueBodyLabel <- getNewLabel
    afterWhileLabel <- getNewLabel
    setAsCurrentLabel ifTrueBodyLabel 

    case stmt of
        (BStmt (Block stmts)) -> do
            compileStmts stmts
        _ -> do
            compileStmt stmt
    emit (Branch conditionLabel)

    conditionLabelFollowUp <- getLabelFollowUp conditionLabel
    emitInSpecificBlock conditionLabelFollowUp (BranchConditional condition ifTrueBodyLabel nextBlock)
    -- setAsCurrentLabel nextBlock
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

    let newType = case type' of
                        c@(ClassType _) -> LLVMTypePointer (LLVMTypePointer (LLVMType type'))
                        _ -> LLVMTypePointer (LLVMType type')
    let allocaVar = (LLVMVariable {
        type' = newType,
        address = LLVMAddressRegister variableRegister,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Alloca allocaVar)
    emit (MemoryStore rhs allocaVar)
    return $ env { vars = Map.insert ident allocaVar (vars env)}
compileDecl declType (Init ident expr) = do
    rhs <- compileExpr expr
    variableRegister <- getNextRegisterCounter
    env <- ask
    store <- get
    let newType = case declType of
                        c@(ClassType _) -> LLVMTypePointer (LLVMTypePointer (LLVMType declType))
                        _ -> LLVMTypePointer (LLVMType declType)
    let allocaVar = (LLVMVariable {
        type' = newType,
        address = LLVMAddressRegister variableRegister,
        blockLabel = currentLabel store,
        ident = Just ident
    })
    emit (Alloca allocaVar)

    case (type' rhs, dereferencePointer $ type' allocaVar) of
        (a, b) | a == b -> emit (MemoryStore rhs allocaVar)
        _ -> do
            newRegister <- getNextRegisterCounter
            blockLabel <- gets currentLabel

            let typeCastResult = (LLVMVariable {
                type' = dereferencePointer $ type' allocaVar,
                address = LLVMAddressRegister newRegister,
                blockLabel = blockLabel,
                ident = Nothing
            })

            emit $ Bitcast typeCastResult (type' rhs) rhs (dereferencePointer $ type' allocaVar)
            emit $ MemoryStore (typeCastResult) allocaVar

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
defaultVariable Boolean = do
    store <- get
    return $ (LLVMVariable {
        type' = LLVMType Boolean,
        address = LLVMAddressImmediate 0,
        blockLabel = currentLabel store,
        ident = Nothing
    })
defaultVariable Str = compileExpr (EString "")
defaultVariable c@(ClassType ident) = do
    store <- get
    return $ (LLVMVariable {
        type' = LLVMTypePointer (LLVMType c),
        address = LLVMAddressNull,
        blockLabel = currentLabel store,
        ident = Nothing
    })

emitDefaultVariableForClassField :: Ident -> LLVMVariable -> (Integer, LLVMClassField) -> GenM()
emitDefaultVariableForClassField cIdent newLLVMVariable (index, cField) = do
    let type' = case classFieldType cField of
                    (LLVMTypePointer (LLVMType t)) -> t
                    (LLVMType t) -> t

    blockLabel <- gets currentLabel
    rhs <- defaultVariable type'

    newRegister <- getNextRegisterCounter
    let result = (LLVMVariable {
        type' = LLVMTypePointer (classFieldType cField),
        address = LLVMAddressRegister newRegister,
        blockLabel = blockLabel,
        ident = Nothing
    })
    emit $ GEPClass result (ClassType cIdent) newLLVMVariable (toInteger index)
    emit $ MemoryStore rhs result


compileExpr :: Expr -> GenM LLVMVariable
-- compileExpr (ELValue (LValue ident)) = do
--     store <- get
--     varsMap <- asks vars
--     let var = fromJust $ Map.lookup ident varsMap
--     newRegisterNumber <- getNextRegisterCounter
--     let registerVar = (LLVMVariable {
--         type' = dereferencePointer $ type' var,
--         address = LLVMAddressRegister newRegisterNumber,
--         blockLabel = currentLabel store,
--         ident = Just ident
--     })
--     emit (Load registerVar var)
--     return registerVar
-- compileExpr (ELValue l@(LValueClassField lvalue ident)) = do
--     lResultMaybe <- getLValue l
--     store <- get
--     case lResultMaybe of
--         Just lResult -> do
--             newRegisterNumber <- getNextRegisterCounter
--             let registerVar = LLVMVariable {
--                 type' = dereferencePointer $ type' lResult,
--                 address = LLVMAddressRegister newRegisterNumber,
--                 blockLabel = currentLabel store,
--                 ident = Nothing
--             }
--             emit (Load registerVar lResult)
--             return registerVar
--         Nothing -> do
--             lResultThis <- getLValue (addThisToLValue lvalue)
--             let lResult = fromJust $ lResultThis
--             newRegisterNumber <- getNextRegisterCounter
--             let registerVar = LLVMVariable {
--                 type' = dereferencePointer $ type' lResult,
--                 address = LLVMAddressRegister newRegisterNumber,
--                 blockLabel = currentLabel store,
--                 ident = Nothing
--             }
--             emit (Load registerVar lResult)
--             return registerVar
compileExpr (ELValue l) = do
    lResultMaybe <- getLValue l
    store <- get
    case lResultMaybe of
        Just lResult -> do
            newRegisterNumber <- getNextRegisterCounter
            let registerVar = LLVMVariable {
                type' = dereferencePointer $ type' lResult,
                address = LLVMAddressRegister newRegisterNumber,
                blockLabel = currentLabel store,
                ident = Nothing
            }
            emit (Load registerVar lResult)
            return registerVar
        Nothing -> do
            lResultThis <- getLValue (addThisToLValue l)
            let lResult = fromJust $ lResultThis
            newRegisterNumber <- getNextRegisterCounter
            let registerVar = LLVMVariable {
                type' = dereferencePointer $ type' lResult,
                address = LLVMAddressRegister newRegisterNumber,
                blockLabel = currentLabel store,
                ident = Nothing
            }
            emit (Load registerVar lResult)
            return registerVar
-- compileExpr (ELValue(LValueArrayElem lvalue expr)) = error "TODO"
compileExpr (ENew type'@(ClassType cIdent)) = do
    store <- get
    blockLabel <- gets currentLabel

    let c = fromJust $ Map.lookup cIdent (classes store)
    let size = calculateClassSize (c)

    -- liftIO $ putStrLn $ printf ("calculated size: %s\n") (show size)

    mallocResult <- getNextRegisterCounter
    emit $ Malloc mallocResult size

    bitcastResult <- getNextRegisterCounter
    emit $ BitcastMalloc bitcastResult mallocResult type'

    let result = LLVMVariable {
        type' = LLVMTypePointer (LLVMType type'),
        address = LLVMAddressRegister bitcastResult,
        blockLabel = (currentLabel store),
        ident = Nothing
    }

    -- TODO: DEFAULT VARIABLES
    let cFields = llvmClassFields $ fromJust $ Map.lookup cIdent (classes store)
    forM_ (zip [0..] cFields) (\(index, cField) -> emitDefaultVariableForClassField cIdent result (index, cField))

    return result
compileExpr (ENullCast type') = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = LLVMTypePointer (LLVMType type'),
        address = LLVMAddressNull,
        blockLabel = blockLabel,
        ident = Nothing
    }
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
        type' = LLVMType Boolean,
        address = LLVMAddressImmediate 1,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr(ELitFalse) = do
    blockLabel <- gets currentLabel
    return LLVMVariable {
        type' = LLVMType Boolean,
        address = LLVMAddressImmediate 0,
        blockLabel = blockLabel,
        ident = Nothing
    }
compileExpr (EApp lvalue@(LValue name) exprs) = do
    env <- ask
    case (currentClass env) of
        Nothing -> compileEApp lvalue exprs
        Just thisClass -> do
            let thisClassMethodIdents = map (\(_, (ClassMethodDef _ ident _ _ )) -> ident) (llvmClassMethods thisClass)

            case find (==name) thisClassMethodIdents of
                Just _ -> compileExpr (EApp (addThisToLValue lvalue) exprs)
                Nothing -> compileEApp lvalue exprs
compileExpr (EApp (LValueClassField lvalue (Ident ident)) exprs) = do
    lvalueVarPointerMaybe <- getLValue lvalue

    lvalueVarPointer <- case lvalueVarPointerMaybe of
            Just lvalueVarPointer -> return lvalueVarPointer
            Nothing -> do
                lvalueVarThisPointerMaybe <- getLValue (addThisToLValue lvalue)
                return $ fromJust $ lvalueVarThisPointerMaybe

    lvalueLoadedReg <- getNextRegisterCounter

    blockLabel <- gets currentLabel
    let lvalueVarLoaded = LLVMVariable {
        type' = dereferencePointer $ type' lvalueVarPointer,
        address = LLVMAddressRegister lvalueLoadedReg,
        blockLabel = blockLabel,
        ident = Nothing
    }
    emit $ (Load lvalueVarLoaded lvalueVarPointer)
    argsWithoutThis <- mapM compileExpr exprs
    store <- get

    let (LLVMTypePointer (LLVMType (ClassType (Ident cIdent)))) = type' lvalueVarLoaded

    -- figure out correct class
    classesMap <- gets classes
    let classMapEntryMethods = llvmClassMethods $ fromJust $ Map.lookup (Ident cIdent)  classesMap
    let index = fromJust $ findIndex (\(_, (ClassMethodDef _ methodIdent _ _)) -> methodIdent == (Ident ident)) classMapEntryMethods
    let (methodClassIdent@(Ident methodClassName), _) = classMapEntryMethods!!index

    -- thisVar <- case (methodClassIdent, (Ident cIdent)) of
    --                 (a, b) | a == b -> return lvalueVarLoaded
    --                 _ -> do -- cast needed
    --                     newRegister <- getNextRegisterCounter
    --                     blockLabel <- gets currentLabel

    --                     let typeCastThis = (LLVMVariable {
    --                         type' = LLVMTypePointer (LLVMType (ClassType methodClassIdent)),
    --                         address = LLVMAddressRegister newRegister,
    --                         blockLabel = blockLabel,
    --                         ident = Nothing
    --                     })

    --                     emit $ Bitcast typeCastThis (type' lvalueVarLoaded) lvalueVarLoaded (type' typeCastThis)
    --                     return typeCastThis

    let args = [lvalueVarLoaded] ++ argsWithoutThis
    let name = Ident (methodClassName ++ "__" ++ ident)
    let func = fromJust $ Map.lookup name (functions store)
    case func of
        (Fun Void fArgs) -> do
            typeCastedArgs <- typeCastArgs fArgs args
            emit (CallVoid name (typeCastedArgs))
            return (LLVMVariable {
                type' = LLVMType Void,
                address = LLVMAddressVoid,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
        (Fun type' fArgs) -> do
            typeCastedArgs <- typeCastArgs fArgs args
            newRegister <- getNextRegisterCounter
            let resultType = case type' of
                                    (ClassType _) -> LLVMTypePointer (LLVMType type')
                                    _ -> LLVMType type'
            let result = (LLVMVariable {
                type' = resultType,
                address = LLVMAddressRegister newRegister,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
            emit (Call result name (typeCastedArgs))
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
        type' = LLVMType Boolean,
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
        type' = LLVMType Boolean,
        address = LLVMAddressRegister nextRegister,
        blockLabel = currentLabel store,
        ident = Nothing
    })

    case (type' left, type' right) of
        (LLVMType Str, _ ) -> case relOp of
                    EQU -> emit $ (Call result (Ident "__compareStringsEQ") [left, right])
                    NE -> emit $ (Call result (Ident "__compareStringsNE") [left, right])
                    _ -> return ()
        (LLVMTypePointer (LLVMType (ClassType l)), LLVMTypePointer (LLVMType (ClassType r))) | l /= r -> do
            rExtendsL <- checkIfClassExtends l r
            lExtendsR <- checkIfClassExtends r l

            when (rExtendsL) ( do
                    right' <- typeCastArg ((ClassType l), (right))
                    emit (Operation left (RelBinOp relOp) right' result)
                )
            when (lExtendsR) ( do
                    left' <- typeCastArg ((ClassType r), (left))
                    emit (Operation left' (RelBinOp relOp) right result)
                )
            -- case (rExtendsL, lExtendsR) of
            --         (True, _) -> do
            --             right' <- typeCastArg (ClassType l) (right)
            --             emit (Operation left (RelBinOp relOp) right' result)
            --         (False, True) ->
            --             left' <- typeCastArg (ClassType r) (left)
            --             emit (Operation left' (RelBinOp relOp) right result)
        _ -> emit (Operation left (RelBinOp relOp) right result)

    -- if (type' left == LLVMType Str) then (do case relOp of
    --                                             EQU -> emit $ (Call result (Ident "__compareStringsEQ") [left, right])
    --                                             NE -> emit $ (Call result (Ident "__compareStringsNE") [left, right])
    --                                             _ -> return ()
    --                                         )
    -- if (type' left == LLVMTypePointer (LLVMType (LLVMC)))
                    
    -- else (emit (Operation left (RelBinOp relOp) right result))


    return result
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
        type' = LLVMType Boolean,
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
        type' = LLVMType Boolean,
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

getLValue :: LValue -> GenM (Maybe LLVMVariable)
getLValue (LValue ident) = do
    varsMap <- asks vars
    return $ Map.lookup ident varsMap
getLValue (LValueClassField lvalue ident) = do
    lvalueVarPointerMaybe <- getLValue lvalue
    case lvalueVarPointerMaybe of
        Nothing -> return Nothing
        Just lvalueVarPointer -> do
            let (LLVMTypePointer (LLVMTypePointer (LLVMType cType@(ClassType cIdent)))) = type' lvalueVarPointer

            blockLabel <- gets currentLabel

            lvalueReg <- getNextRegisterCounter
            let lvalueVar = LLVMVariable {
                type' = (LLVMTypePointer (LLVMType cType)),
                address = LLVMAddressRegister lvalueReg,
                blockLabel = blockLabel,
                ident = Nothing
            }

            emit $ (Load lvalueVar lvalueVarPointer)
            
            classesM <- gets classes
            let classFs = llvmClassFields $ fromJust $ Map.lookup cIdent classesM
            let index = fromJust $ findIndex (\cF -> classFieldName cF == ident) classFs
            let identType = classFieldType $ classFs!!index

            blockLabel <- gets currentLabel

            newRegister <- getNextRegisterCounter
            let result = LLVMVariable {
                type' = LLVMTypePointer identType,
                address = LLVMAddressRegister newRegister,
                blockLabel = blockLabel,
                ident = Nothing
            }

            emit $ GEPClass result cType lvalueVar (toInteger index)
            return $ Just result
getLValue _ = error "todo"

showIdent :: Ident -> String
showIdent (Ident s) = s

showArgs :: [Arg] -> String
showArgs args = intercalate (", ") (map showArg args)

showArg :: Arg -> String
showArg (Arg c@(ClassType _) ident) =  printf ("%s %%%s") (printLLVMType (LLVMTypePointer (LLVMType c))) (showIdent ident)
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
showTypeInLLVM Boolean = "i1"
showTypeInLLVM (ClassType (Ident llvmClassName)) = printf("%%%s") (llvmClassName)
showTypeInLLVM _ = ""

dereferencePointer :: LLVMType -> LLVMType
dereferencePointer (LLVMTypePointer t) = t
dereferencePointer _ = error "not a pointer!"

printLLVMClass :: LLVMClass -> String
printLLVMClass c = (printf ("%%%s = type {\n\t%s\n}\n")
            (showIdent $ llvmClassName c)
            (intercalate (",\n\t") (map printLLVMType (map (\cF -> classFieldType cF) (llvmClassFields c))))
        )

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
printLLVMAddress LLVMAddressNull = "null"
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
    (MulBinOp Times) -> printf ("%s = mul %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (MulBinOp Div) -> printf ("%s = sdiv %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
    (MulBinOp Mod) -> printf ("%s = srem %s %s, %s") (printLLVMVarAddress result) (printLLVMVarType result) (printLLVMVarAddress l) (printLLVMVarAddress r)
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
printLLVMInstruction (Malloc res size) = printf ("%%r%s = call i8* @malloc(i32 %s)") (show res) (show size)
printLLVMInstruction (BitcastMalloc res mallocAddress (ClassType (Ident i))) = printf ("%%r%s = bitcast i8* %%r%s to %%%s*") (show res) (show mallocAddress) (i)
printLLVMInstruction (GEPClass rVar (ClassType (Ident cType)) lVar index) = printf("%s = getelementptr %%%s, %%%s* %s, i32 0, i32 %s") (printLLVMVarAddress rVar) (cType) (cType) (printLLVMVarAddress lVar) (show index)
printLLVMInstruction (Bitcast resultVar fromType lVar toType) = printf("%s = bitcast %s %s to %s") (printLLVMVarAddress resultVar) (printLLVMType fromType) (printLLVMVarAddress lVar) (printLLVMType toType)
printPhiVars :: [LLVMVariable] -> String
printPhiVars vars = intercalate (", ") (map (\var -> (printf ("[ %s, %%%s ]") (printLLVMVarAddress var) (printLLVMVarLabel var))) vars)

predefinedFunctions :: [(Ident, Type)]
predefinedFunctions = ([
        (Ident "printInt", (Fun Void [Int])),
        (Ident "printString", (Fun Void [Str])),
        (Ident "error", (Fun Void [])),
        (Ident "readInt", (Fun Int [])),
        (Ident "readString", (Fun Str [])),
        (Ident "__concatStrings", (Fun Str [Str, Str])),
        (Ident "__compareStringsEQ", (Fun Boolean [Str, Str])),
        (Ident "__compareStringsNE", (Fun Boolean [Str, Str]))

    ])

showPredefinedFunctions :: String
showPredefinedFunctions = (intercalate ("\n") ([
        "declare i8* @malloc(i32) nounwind",
        "declare void @printInt(i32)",
        "declare void @printString(i8*)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()",
        "declare i8* @__concatStrings(i8*, i8*)",
        "declare i1 @__compareStringsEQ(i8*, i8*)",
        "declare i1 @__compareStringsNE(i8*, i8*)"
    ])) ++ "\n"

addThisToLValue :: LValue -> LValue
addThisToLValue (LValue ident) = LValueClassField (LValue thisIdent) (ident)
addThisToLValue (LValueClassField lvalue ident) = LValueClassField (addThisToLValue lvalue) (ident)

topIdentLValue :: LValue -> Ident
topIdentLValue (LValue ident) = ident
topIdentLValue (LValueClassField lvalue _) = topIdentLValue lvalue

compileEApp :: LValue -> [Expr] -> GenM LLVMVariable
compileEApp (LValue name) exprs = do
    args <- mapM compileExpr exprs
    store <- get
    let func = fromJust $ Map.lookup name (functions store)
    case func of
        (Fun Void fArgs) -> do
            typeCastedArgs <- typeCastArgs fArgs args
            emit (CallVoid name typeCastedArgs)
            return (LLVMVariable {
                type' = LLVMType Void,
                address = LLVMAddressVoid,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
        (Fun type' fArgs) -> do
            newRegister <- getNextRegisterCounter
            let resultType = case type' of
                                    (ClassType _) -> LLVMTypePointer (LLVMType type')
                                    _ -> LLVMType type'
            let result = (LLVMVariable {
                type' = resultType,
                address = LLVMAddressRegister newRegister,
                blockLabel = (currentLabel store),
                ident = Nothing
            })
            typeCastedArgs <- typeCastArgs fArgs args
            emit (Call result name typeCastedArgs)
            return result

checkIfClassExtends :: Ident -> Ident -> GenM Bool
checkIfClassExtends baseClassIdent derivedClassIdent = do
    classesMap <- gets classes

    case (Map.lookup derivedClassIdent classesMap) of
        Nothing -> throwError $ CompilationError ("Class not in scope " ++ show derivedClassIdent)
        Just derivedClass -> case (llvmBaseClassName derivedClass) of
                                Nothing -> return False
                                (Just baseClassForThisDerivedClass) | baseClassForThisDerivedClass == baseClassIdent -> return True
                                (Just baseClassForThisDerivedClass) -> checkIfClassExtends baseClassIdent baseClassForThisDerivedClass


typeCastArg :: (Type, LLVMVariable) -> GenM LLVMVariable
typeCastArg (argType, llvmVariable) = do
    case ((type' llvmVariable), (LLVMType argType)) of
        (a, b) | a == b -> return llvmVariable
        _ -> do
            newRegister <- getNextRegisterCounter
            blockLabel <- gets currentLabel

            let typeCasted = (LLVMVariable {
                type' = LLVMTypePointer (LLVMType argType),
                address = LLVMAddressRegister newRegister,
                blockLabel = blockLabel,
                ident = Nothing
            })

            emit $ Bitcast typeCasted (type' llvmVariable) llvmVariable (type' typeCasted)
            return typeCasted
typeCastArgs :: [Type] -> [LLVMVariable] -> GenM ([LLVMVariable])
typeCastArgs fArgs args = do
    llvmVariables <- mapM typeCastArg (zip fArgs args)
    return llvmVariables