module ASTOptimizer where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Bool

import AbsLatte

type OM a = (ReaderT Env (StateT Store IO)) a

runASTOptimizer :: (Optimizable program) => program -> IO Program
runASTOptimizer program = do
    (res, store) <- runStateT (runReaderT (optimize program) initEnv) initStore
    return res

class Optimizable f where
    optimize :: f -> OM Program

data Env = Env {

}

initEnv = Env {

}

data Store = Store {
    -- functions :: Map.Map Ident (Type, Bool)
    -- functionsOptimized :: Map.Map Ident (Block)
}

initStore = Store {
    -- functions = Map.fromList (predefinedFunctions)
}

instance Optimizable Program where
    optimize program@(Program topdefs) = do
        forM_ topdefs fillFunctionInformation
        optimizedTopDefs <- mapM optimizeTopDef topdefs
        return $ Program optimizedTopDefs

fillFunctionInformation :: TopDef -> OM ()
fillFunctionInformation _ = return ()
-- fillFunctionInformation (FnDef type' ident args block) = do
--     let hasEffect = case type' of
--             Void -> functionHasIO block
--             _v -> True
--     modify (\store -> store {
--         functions = Map.insert ident ((Fun type' (map (\(Arg t _) -> t) args)), True) (functions store)
--     })

optimizeTopDef :: TopDef -> OM TopDef
optimizeTopDef (FnDef Void ident args (Block [])) = do
    return (FnDef Void ident args (Block [VRet]))
optimizeTopDef (FnDef type' ident args block@(Block stmts)) = do
    optimizedStmts <- optimizeStmts stmts
    return (FnDef type' ident args (Block optimizedStmts))

optimizeStmts :: [Stmt] -> OM [Stmt]
optimizeStmts [] = return []
optimizeStmts (x@(Ret e):_) = return [x]
optimizeStmts (x@(VRet):_) = return [x]
optimizeStmts(Empty:xs) = optimizeStmts xs
optimizeStmts (x:xs) = do
    optimizedX <- optimizeStmt x 
    optimizedXS <- optimizeStmts xs
    return $ [optimizedX] ++ optimizedXS

optimizeStmt :: Stmt -> OM Stmt
optimizeStmt s@(BStmt (Block stmts)) = do
    stmtso <- optimizeStmts stmts
    case stmtso of
        [] -> return Empty
        _ -> return s
optimizeStmt s@(SExp e) = do
    -- todo: check if theres func app with side effects
    eo <- optimizeExpr e
    return (SExp eo)

optimizeStmt s@(Cond e sTrue) = do
    eo <- optimizeExpr e
    case eo of
        ELitTrue -> return sTrue
        ELitFalse -> return Empty
        _ -> return s

optimizeStmt s@(CondElse e sTrue sFalse) = do
    eo <- optimizeExpr e
    case eo of
        ELitTrue -> return sTrue
        ELitFalse -> return sFalse
        _ -> return s
optimizeStmt s@(While e stmts) = do
    eo <- optimizeExpr e
    case eo of
        ELitFalse -> return Empty
        _ -> return s
optimizeStmt s = return s

optimizeExpr :: Expr -> OM Expr
optimizeExpr e@(Not e1) = do
    eo1 <- optimizeExpr e1

    case eo1 of
        (ELitFalse) -> return ELitTrue
        (ELitTrue) -> return ELitFalse
        _ -> return e
optimizeExpr e@(EMul e1 mulOp e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2

    case (eo1, eo2) of
        (ELitInt i, ELitInt j) -> do
                let func = case mulOp of
                        Times -> (*)
                        Div -> (div)
                        Mod -> (mod)
                return $ ELitInt (i `func` j)
        _ -> return e
optimizeExpr e@(EAdd e1 addOp e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2

    case (eo1, eo2) of
        (ELitInt i, ELitInt j) -> do
                let func = case addOp of
                        Plus -> (+)
                        Minus -> (-)
                return $ ELitInt (i `func` j)
        _ -> return e
optimizeExpr e@(ERel e1 EQU e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2

    case (eo1, eo2) of
        (ELitFalse, ELitFalse) -> return ELitTrue
        (ELitTrue, ELitTrue) -> return ELitTrue
        (ELitFalse, _) -> return ELitFalse
        (ELitTrue, _) -> return ELitFalse
        (ELitInt i, ELitInt j) | i == j -> return ELitTrue
        (ELitInt i, ELitInt j) | i /= j -> return ELitFalse
        _ -> return e
optimizeExpr e@(ERel e1 NE e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2

    case (eo1, eo2) of
        (ELitFalse, ELitFalse) -> return ELitFalse
        (ELitTrue, ELitTrue) -> return ELitFalse
        (ELitFalse, _) -> return ELitTrue
        (ELitTrue, _) -> return ELitTrue
        (ELitInt i, ELitInt j) | i == j -> return ELitFalse
        (ELitInt i, ELitInt j) | i /= j -> return ELitTrue
        _ -> return e
optimizeExpr e@(ERel e1 relOp e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2
    case (eo1, eo2) of
        (ELitInt i, ELitInt j) -> do 
                let result = case relOp of
                        LTH -> i < j
                        LE -> i <= j
                        GTH -> i > j
                        GE -> i >= j
                case result of
                    True -> return ELitTrue
                    False -> return ELitFalse
        _ -> return e

optimizeExpr e@(EAnd e1 e2) = do
    eo1 <- optimizeExpr e1
    case eo1 of
        ELitFalse -> return ELitFalse
        _ -> return e
optimizeExpr e@(EOr e1 e2) = do
    eo1 <- optimizeExpr e1
    case eo1 of
        ELitTrue -> return ELitTrue
        _ -> return e
optimizeExpr e = return e

-- predefinedFunctions :: [(Ident, (Type, Bool))]
-- predefinedFunctions = ([
--         (Ident "printInt", ((Fun Void [Int]), True)),
--         (Ident "printString", ((Fun Void [Str]), True)),
--         (Ident "error", ((Fun Void []), True)),
--         (Ident "readInt", ((Fun Int []), True)),
--         (Ident "readString", ((Fun Str []), True)),
--         (Ident "__concatStrings", ((Fun Str [Str, Str]), True))
--     ])

---
-- exprHasPossibleIO :: Expr -> Bool
-- exprHasPossibleIO (EApp (Ident fnName) args) = (fnName `elem` ([
--         "printInt",
--         "printString",
--         "error",
--         "readInt",
--         "readString"
--     ])) || any exprHasPossibleIO args
-- exprHasPossibleIO (Neg e) = exprHasPossibleIO e
-- exprHasPossibleIO (Not e) = exprHasPossibleIO e
-- exprHasPossibleIO (EMul e1 _ e2) = exprHasPossibleIO e1 || exprHasPossibleIO e2
-- exprHasPossibleIO (EAdd e1 _ e2) = exprHasPossibleIO e1 || exprHasPossibleIO e2
-- exprHasPossibleIO (ERel e1 _ e2) = exprHasPossibleIO e1 || exprHasPossibleIO e2
-- exprHasPossibleIO (EAnd e1 e2) = exprHasPossibleIO e1 || exprHasPossibleIO e2
-- exprHasPossibleIO (EOr e1 e2) = exprHasPossibleIO e1 || exprHasPossibleIO e2
-- functionHasIOExpr _ = False