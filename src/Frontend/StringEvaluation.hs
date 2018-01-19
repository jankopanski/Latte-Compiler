module Frontend.StringEvaluation where

import Control.Monad.State
import Control.Monad.Reader
import Data.Set hiding (foldl, map)
import qualified Data.Map as Map

import Parser.AbsLatte
import Frontend.Globals


type ClsEnv = Map.Map Ident (Map.Map Ident (Type Position))
-- Set of string variables, functions that return string and arrays of type string
type Evaluator a = ReaderT ClsEnv (State (Set Ident)) a
type StatementEvaluator = Evaluator (Stmt Position)
-- (Is expression type a string, evaulated expression)
type ExpressionEvaluator = Evaluator (Bool, Expr Position)

isString :: Ident -> Evaluator Bool
isString ident = get >>= \s -> return $ member ident s

addString :: Ident -> Evaluator ()
addString ident = state (\s -> ((), insert ident s))

removeString :: Ident -> Evaluator ()
removeString ident = state (\s -> ((), delete ident s))

-- Top functions --

runStringEvaluation :: Program Position -> IO (Program Position)
runStringEvaluation program =
  return $ evalState (runReaderT (strEvalProgram program) Map.empty) empty

strEvalProgram :: Program Position -> Evaluator (Program Position)
strEvalProgram (Program pos topdefs) = do
  let funs = foldl addFunction empty inbuildFunctions
      funs' = foldl addFunction funs topdefs
      clsenv = strCollectClasses topdefs
  put funs'
  -- topdefs' <- mapM (local (const clsenv) . strEvalTopDef) topdefs
  topdefs' <- local (const clsenv) (mapM strEvalFunction topdefs)
  return (Program pos topdefs')
  where
    addFunction :: Set Ident -> TopDef Position -> Set Ident
    addFunction set (FnDef _ (Str _) ident _ _) = insert ident set
    addFunction set _ = set

strCollectClasses :: [TopDef Position] -> ClsEnv
strCollectClasses = foldl strAddClass Map.empty where
  strAddClass :: ClsEnv -> TopDef Position -> ClsEnv
  strAddClass clsenv (ClsDef _ ident fields) =
    let fieldMap = foldl (\fieldMap' (Field _ t fident) ->
          Map.insert fident t fieldMap') Map.empty fields
    in Map.insert ident fieldMap clsenv
  strAddClass clsenv _ = clsenv

strEvalFunction :: TopDef Position -> Evaluator (TopDef Position)
strEvalFunction (FnDef pos t ident args block) =
  strEvalBlock block >>= \b -> return (FnDef pos t ident args b)
strEvalFunction def = return def

strEvalBlock :: Block Position -> Evaluator (Block Position)
strEvalBlock (Block pos stmts) =
  mapM strEvalStmt stmts >>= \stmts' -> return (Block pos stmts')

-- Statements --

strEvalStmt :: Stmt Position -> StatementEvaluator

strEvalStmt (BStmt pos block) = do
  s <- get
  b <- strEvalBlock block
  put s
  return (BStmt pos b)

strEvalStmt (Decl pos t items) =
  mapM mapItem items >>= \items' -> return (Decl pos t items')
  where
    mapOper = case t of
      Str _ -> addString
      Arr _ (Str _) -> addString
      _ -> removeString
    mapItem :: Item Position -> Evaluator (Item Position)
    mapItem i@(NoInit _ ident) = mapOper ident >> return i
    mapItem (Init pos' ident expr) =
      mapOper ident >> strEvalExpr expr >>= \(_, e) -> return (Init pos' ident e)

strEvalStmt (Ass pos ident expr) =
  strEvalExpr expr >>= \(_, e) -> return (Ass pos ident e)

strEvalStmt (ArrAss pos ident expr1 expr2) =
  strEvalExpr expr2 >>= \(_, e) -> return (ArrAss pos ident expr1 e)

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

strEvalStmt (For pos t ident1 ident2 stmt) =
  strEvalStmt stmt >>= \s -> return $ For pos t ident1 ident2 s

strEvalStmt (SExp pos expr) = strEvalExpr expr >>= \(_, e) -> return (SExp pos e)

strEvalStmt stmt = return stmt

-- Expressions --

isFieldString :: [Var Position] -> Evaluator Bool
isFieldString [] = return False
isFieldString (Var pos ident : vars) = do
  finaltype <- foldM resolveVar (Cls pos ident) vars
  return (finaltype == Str pos)
  where
    resolveVar :: Type Position -> Var Position -> Evaluator (Type Position)
    resolveVar (Cls _ clsid) (Var _ fieldid) = ask >>= \clsenv ->
      return $ (clsenv Map.! clsid) Map.! fieldid
    resolveVar t _ = return t

strEvalExpr :: Expr Position -> ExpressionEvaluator

strEvalExpr e@(EVar _ [Var _ ident]) = isString ident >>= \b -> return (b, e)
strEvalExpr e@(EVar _ vars) = isFieldString vars >>= \b -> return (b, e)

strEvalExpr (EApp pos ident exprs) = do
  b <- isString ident
  evals <- mapM strEvalExpr exprs
  return (b, EApp pos ident (map snd evals))

strEvalExpr e@EString{} = return (True, e)

strEvalExpr (ENewArr pos t expr) =
  strEvalExpr expr >>= \(_, e) -> return (False, ENewArr pos t e)

strEvalExpr (EAccArr pos ident expr) =
  isString ident >>= \b -> return (b, EAccArr pos ident expr)

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
