module Frontend.StringEvaluation where

import Control.Monad.State
import Data.Set hiding (foldl)

import Parser.AbsLatte
import Frontend.Globals

type Evaluator a = State (Set Ident) a
type StatementEvaluator = Evaluator (Stmt Position)
type ExpressionEvaluator = Evaluator (Bool, Expr Position)

isString :: Ident -> Evaluator Bool
isString ident = get >>= \s -> return $ member ident s

addString :: Ident -> Evaluator ()
addString ident = state (\s -> ((), insert ident s))

removeString :: Ident -> Evaluator ()
removeString ident = state (\s -> ((), delete ident s))

-- Top functions --

runStringEvaluation :: Program Position -> IO (Program Position)
runStringEvaluation program = return $ evalState (strEvalProgram program) empty

strEvalProgram :: Program Position -> Evaluator (Program Position)
strEvalProgram (Program pos topdefs) = do
  let funs = foldl addFunction empty topdefs
  put funs
  topdefs' <- mapM strEvalTopDef topdefs
  return (Program pos topdefs')
  where
    addFunction :: Set Ident -> TopDef Position -> Set Ident
    addFunction set (FnDef _ (Str _) ident _ _) = insert ident set
    addFunction set _ = set

strEvalTopDef :: TopDef Position -> Evaluator (TopDef Position)
strEvalTopDef (FnDef pos t ident args block) =
  strEvalBlock block >>= \b -> return (FnDef pos t ident args b)

strEvalBlock :: Block Position -> Evaluator (Block Position)
strEvalBlock (Block pos stmts) =
  mapM strEvalStmt stmts >>= \stmts' -> return (Block pos stmts')

-- Statements --

strEvalStmt :: Stmt Position -> StatementEvaluator

-- strEvalStmt (BStmt _ block) = do

strEvalStmt (BStmt pos block) = do
  s <- get
  b <- strEvalBlock block
  put s
  return (BStmt pos b)

strEvalStmt (Decl pos t@(Str _) items) =
  mapM mapItem items >>= \items' -> return (Decl pos t items') where
    mapItem :: Item Position -> Evaluator (Item Position)
    mapItem i@(NoInit _ ident) = addString ident >> return i
    mapItem (Init pos' ident expr) =
      addString ident >> strEvalExpr expr >>= \(_, e) -> return (Init pos' ident e)

strEvalStmt (Decl pos t items) =
  mapM mapItem items >>= \items' -> return (Decl pos t items') where
    mapItem i@(NoInit _ ident) = removeString ident >> return i
    mapItem (Init pos' ident expr) =
      removeString ident >> strEvalExpr expr >>= \(_, e) -> return (Init pos' ident e)

strEvalStmt (Ass pos ident expr) =
  strEvalExpr expr >>= \(_, e) -> return (Ass pos ident e)

strEvalStmt (Ret pos expr) = strEvalExpr expr >>= \(_, e) -> return (Ret pos e)

strEvalStmt (Cond pos expr stmt) = do
  (_, e) <- strEvalExpr expr
  s <- strEvalStmt stmt
  return (Cond pos e s)

strEvalStmt (CondElse pos expr stmt1 stmt2) = do
  (_, e) <- strEvalExpr expr
  s1 <- strEvalStmt stmt1
  s2 <- strEvalStmt stmt2
  return (CondElse pos e s1 s2)

strEvalStmt (While pos expr stmt) = do
  (_, e) <- strEvalExpr expr
  s <- strEvalStmt stmt
  return (While pos e s)

strEvalStmt (SExp pos expr) = strEvalExpr expr >>= \(_, e) -> return (SExp pos e)

strEvalStmt stmt = return stmt

-- Expressions --

strEvalExpr :: Expr Position -> ExpressionEvaluator

strEvalExpr e@(EVar _ ident) = isString ident >>= \b -> return (b, e)

strEvalExpr e@(EApp _ ident _) = isString ident >>= \b -> return (b, e)

strEvalExpr e@EString{} = return (True, e)

strEvalExpr (Not pos expr) =
  strEvalExpr expr >>= \(_, e) -> return (False, Not pos e)

strEvalExpr (EAdd pos expr1 op@(Plus _) expr2) = do
  (b1, e1) <- strEvalExpr expr1
  (b2, e2) <- strEvalExpr expr2
  return $ if b1 || b2
    then (True, EApp pos (Ident "_concatString") [e1, e2])
    else (False, EAdd pos e1 op e2)

strEvalExpr (ERel pos expr1 op@(EQU _) expr2) = do
  (b1, e1) <- strEvalExpr expr1
  (b2, e2) <- strEvalExpr expr2
  return $ if b1 || b2
    then (True, EApp pos (Ident "_cmpString") [e1, e2])
    else (False, ERel pos e1 op e2)

strEvalExpr (ERel pos expr1 op@(NE pos') expr2) = do
  (b1, e1) <- strEvalExpr expr1
  (b2, e2) <- strEvalExpr expr2
  return $ if b1 || b2
    then (True, Not pos (EApp pos' (Ident "_cmpString") [e1, e2]))
    else (False, ERel pos e1 op e2)

strEvalExpr (ERel pos expr1 op expr2) = do
  (_, e1) <- strEvalExpr expr1
  (_, e2) <- strEvalExpr expr2
  return (False, ERel pos e1 op e2)

strEvalExpr (EAnd pos expr1 expr2) = do
  (_, e1) <- strEvalExpr expr1
  (_, e2) <- strEvalExpr expr2
  return (False, EAnd pos e1 e2)

strEvalExpr (EOr pos expr1 expr2) = do
  (_, e1) <- strEvalExpr expr1
  (_, e2) <- strEvalExpr expr2
  return (False, EOr pos e1 e2)

strEvalExpr expr = return (False, expr)
