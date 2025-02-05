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
    level :: Integer,
    vars :: Map.Map Ident (Type, Integer)
}

initEnv = Env {
    level = 0,
    vars = Map.empty
}

data Store = Store {
}

initStore = Store {
}


instance Optimizable Program where
    optimize program@(Program topdefs) = do
        forM_ topdefs fillFunctionInformation
        optimizedTopDefs <- mapM optimizeTopDef topdefs
        return $ Program optimizedTopDefs

fillFunctionInformation :: TopDef -> OM ()
fillFunctionInformation _ = return ()

optimizeTopDef :: TopDef -> OM TopDef
optimizeTopDef (FnDef Void ident args (Block [])) = do
    return (FnDef Void ident args (Block [VRet]))
optimizeTopDef (FnDef type' ident args block@(Block stmts)) = do
    optimizedStmts <- optimizeStmts stmts
    return (FnDef type' ident args (Block optimizedStmts))
optimizeTopDef f = return f

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
        _ -> return (BStmt (Block stmtso))
optimizeStmt s@(SExp e) = do
    eo <- optimizeExpr e
    return (SExp eo)

optimizeStmt s@(Cond e sTrue) = do
    eo <- optimizeExpr e
    so <- optimizeStmt sTrue
    case so of 
        Empty -> return Empty
        _ -> case eo of
            ELitTrue -> return so
            ELitFalse -> return Empty
            _ -> return (Cond eo so)

optimizeStmt s@(CondElse e sTrue sFalse) = do
    eo <- optimizeExpr e
    soT <- optimizeStmt sTrue
    soF <- optimizeStmt sFalse
    case eo of
        ELitTrue -> return soT
        ELitFalse -> return soF
        _ -> return (CondElse eo soT soF)
optimizeStmt s@(While e stmt) = do
    eo <- optimizeExpr e
    so <- optimizeStmt stmt
    case eo of
        ELitFalse -> return Empty
        _ -> return (While eo so)
optimizeStmt s = return s

optimizeExpr :: Expr -> OM Expr
optimizeExpr e@(Not e1) = do
    eo1 <- optimizeExpr e1

    case eo1 of
        (ELitFalse) -> return ELitTrue
        (ELitTrue) -> return ELitFalse
        _ -> return (Not eo1)
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
        _ -> return (EMul eo1 mulOp eo2)
optimizeExpr e@(EAdd e1 addOp e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2

    case (eo1, eo2) of
        (ELitInt i, ELitInt j) -> do
                let func = case addOp of
                        Plus -> (+)
                        Minus -> (-)
                return $ ELitInt (i `func` j)
        _ -> return (EAdd eo1 addOp eo2)
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
        _ -> return (ERel eo1 EQU eo2)
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
        _ -> return (ERel eo1 NE eo2)
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
        _ -> return (ERel eo1 relOp eo2)

optimizeExpr e@(EAnd e1 e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2
    case eo1 of
        ELitFalse -> return ELitFalse
        ELitTrue -> return eo2
        _ -> return (EAnd eo1 eo2)
optimizeExpr e@(EOr e1 e2) = do
    eo1 <- optimizeExpr e1
    eo2 <- optimizeExpr e2
    case eo1 of
        ELitTrue -> return ELitTrue
        ELitFalse -> return eo2
        _ -> return (EOr eo1 eo2)
optimizeExpr e = return e