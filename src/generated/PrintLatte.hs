{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintLatte.
--   Generated by the BNF converter.

module PrintLatte where

import qualified AbsLatte
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsLatte.Ident where
  prt _ (AbsLatte.Ident i) = doc (showString i)

instance Print AbsLatte.Program where
  prt i e = case e of
    AbsLatte.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print AbsLatte.TopDef where
  prt i e = case e of
    AbsLatte.FnDef type_ id args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
    AbsLatte.ClassDef id classpoles -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id, doc (showString "{"), prt 0 classpoles, doc (showString "}")])
    AbsLatte.ClassDefExt id1 id2 classpoles -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id1, doc (showString "extends"), prt 0 id2, doc (showString "{"), prt 0 classpoles, doc (showString "}")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsLatte.ClassPole where
  prt i e = case e of
    AbsLatte.ClassFieldDef type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString ";")])
    AbsLatte.ClassMethodDef type_ id args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsLatte.TopDef] where
  prt = prtList

instance Print [AbsLatte.ClassPole] where
  prt = prtList

instance Print AbsLatte.Arg where
  prt i e = case e of
    AbsLatte.Arg type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLatte.Arg] where
  prt = prtList

instance Print AbsLatte.Block where
  prt i e = case e of
    AbsLatte.Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsLatte.Stmt] where
  prt = prtList

instance Print AbsLatte.Stmt where
  prt i e = case e of
    AbsLatte.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsLatte.BStmt block -> prPrec i 0 (concatD [prt 0 block])
    AbsLatte.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsLatte.Ass lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsLatte.Incr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    AbsLatte.Decr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    AbsLatte.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsLatte.VRet -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsLatte.Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLatte.CondElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsLatte.While expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLatte.ForArray type_ id1 id2 -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 type_, prt 0 id1, doc (showString ":"), prt 0 id2, doc (showString ")")])
    AbsLatte.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsLatte.Item where
  prt i e = case e of
    AbsLatte.NoInit lvalue -> prPrec i 0 (concatD [prt 0 lvalue])
    AbsLatte.Init lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLatte.Item] where
  prt = prtList

instance Print AbsLatte.LValue where
  prt i e = case e of
    AbsLatte.LValue id -> prPrec i 0 (concatD [prt 0 id])
    AbsLatte.LValueClassField lvalue id -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "."), prt 0 id])
    AbsLatte.LValueArrayElem lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AbsLatte.Type where
  prt i e = case e of
    AbsLatte.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsLatte.Str -> prPrec i 0 (concatD [doc (showString "string")])
    AbsLatte.Boolean -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsLatte.Void -> prPrec i 0 (concatD [doc (showString "void")])
    AbsLatte.ArrType type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    AbsLatte.ClassType id -> prPrec i 0 (concatD [prt 0 id])
    AbsLatte.Fun type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLatte.Type] where
  prt = prtList

instance Print AbsLatte.Expr where
  prt i e = case e of
    AbsLatte.EField lvalue -> prPrec i 8 (concatD [prt 0 lvalue])
    AbsLatte.ELitInt n -> prPrec i 8 (concatD [prt 0 n])
    AbsLatte.ELitTrue -> prPrec i 8 (concatD [doc (showString "true")])
    AbsLatte.ELitFalse -> prPrec i 8 (concatD [doc (showString "false")])
    AbsLatte.EApp lvalue exprs -> prPrec i 8 (concatD [prt 0 lvalue, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsLatte.EString str -> prPrec i 8 (concatD [prt 0 str])
    AbsLatte.ENewArray type_ expr -> prPrec i 7 (concatD [doc (showString "new"), prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
    AbsLatte.ENew type_ -> prPrec i 7 (concatD [doc (showString "new"), prt 0 type_])
    AbsLatte.ENullCast type_ -> prPrec i 6 (concatD [doc (showString "("), prt 0 type_, doc (showString ")null")])
    AbsLatte.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsLatte.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsLatte.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsLatte.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsLatte.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsLatte.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsLatte.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLatte.Expr] where
  prt = prtList

instance Print AbsLatte.AddOp where
  prt i e = case e of
    AbsLatte.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsLatte.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsLatte.MulOp where
  prt i e = case e of
    AbsLatte.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsLatte.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsLatte.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsLatte.RelOp where
  prt i e = case e of
    AbsLatte.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsLatte.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsLatte.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsLatte.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsLatte.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsLatte.NE -> prPrec i 0 (concatD [doc (showString "!=")])

