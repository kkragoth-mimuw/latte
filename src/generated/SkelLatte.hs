module SkelLatte where

-- Haskell module generated by the BNF converter

import AbsLatte
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x
transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type_ ident args block -> failure x
  ClassDef ident classpoles -> failure x
  ClassDefExt ident1 ident2 classpoles -> failure x
transClassPole :: ClassPole -> Result
transClassPole x = case x of
  ClassFieldDef type_ ident -> failure x
  ClassMethodDef type_ ident args block -> failure x
transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass lvalue expr -> failure x
  Incr lvalue -> failure x
  Decr lvalue -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  SExp expr -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
transLValue :: LValue -> Result
transLValue x = case x of
  LValue ident -> failure x
  LValueClassField lvalue ident -> failure x
transType :: Type -> Result
transType x = case x of
  Int -> failure x
  Str -> failure x
  Boolean -> failure x
  Void -> failure x
  ClassType ident -> failure x
  Fun type_ types -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  ELValue lvalue -> failure x
  ELitInt integer -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  EApp lvalue exprs -> failure x
  EString string -> failure x
  ENew type_ -> failure x
  ENullCast type_ -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x

