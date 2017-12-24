module StaticAnalysis where

-- Imports --

import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import System.Exit (exitFailure)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AbsLatte

-- Data types --

type Name = String
type Position = Maybe (Int, Int)
type TypeEnv = Map.Map Name (Type Position)
type TypeScope = (TypeEnv, TypeEnv)

data Error
  = Redefinition Name Position Position
  | TypeMismatch Name (Type Position) (Type Position)
  | TypeMismatchAnonymous (Type Position)
  | UndefinedVariable Name Position
  | UndefinedFunction Name Position
  | VariableCall Name Position
  | InvalidNumberOfArguments Name Position
  | InvalidArgumentType Name Position
  | InvalidDeclarationType Position
  | InvalidReturnType (Type Position)
  | UnexpectedError Position
instance Show Error where
  show (Redefinition name new old) =
    showErrorPosition new ++ "Redefinition of '" ++ name ++
      "'\n\tPreviously defined at " ++ showPosition old
  show (TypeMismatch name actual expected) =
    showErrorPosition (getPositionFromType actual) ++ "Type mismatch, used type " ++
      show actual ++ ", expected " ++ show expected ++ ", " ++ name  ++
      " was declared at " ++ showPosition (getPositionFromType expected)
  show (TypeMismatchAnonymous t) =
    showErrorPosition (getPositionFromType t) ++ "Expected type " ++ show t
  show (UndefinedFunction name pos) =
    showErrorPosition pos ++ "Undefined function " ++ name
  show (UndefinedVariable name pos) =
    showErrorPosition pos ++ "Undefined variable " ++ name
  show (VariableCall name pos) =
    showErrorPosition pos ++ name ++ " is not callable"
  show (InvalidNumberOfArguments name pos) =
    showErrorPosition pos ++ "Invalid number of arguments in call to function " ++ name
  show (InvalidArgumentType name pos) =
    showErrorPosition pos ++ "Invalid type of argument in call to function " ++ name
  show (InvalidDeclarationType pos) =
    showErrorPosition pos ++ "Invalid declaration type"
  show (InvalidReturnType t) =
    showErrorPosition (getPositionFromType t) ++ "Invalid type of return, expected " ++
    show t
  show (UnexpectedError pos) =
    showErrorPosition pos ++ "Unexpected error"

type Checker a = ExceptT Error (State TypeScope) a
type StatementChecker a = ReaderT (Type Position) (ExceptT Error (State TypeScope)) a
type ExpressionChecker = StatementChecker (Type Position)

-- Helper functions --

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = "line " ++ show line ++ ", position " ++ show column

showErrorPosition :: Position -> String
showErrorPosition pos = showPosition pos ++ ": "

inbuildFunctions :: [TopDef Position]
inbuildFunctions =
  [
  FnDef Nothing (Void Nothing) (Ident "printInt")
    [Arg Nothing (Int Nothing) (Ident "")] (Block Nothing []),
  FnDef Nothing (Void Nothing) (Ident "printString")
    [Arg Nothing (Str Nothing) (Ident "")] (Block Nothing []),
  FnDef Nothing (Void Nothing) (Ident "error") [] (Block Nothing []),
  FnDef Nothing (Int Nothing) (Ident "readInt") [] (Block Nothing []),
  FnDef Nothing (Str Nothing) (Ident "readString") [] (Block Nothing [])
  ]

getPositionFromType :: Type Position -> Position
getPositionFromType (Int pos)     = pos
getPositionFromType (Str pos)     = pos
getPositionFromType (Bool pos)    = pos
getPositionFromType (Void pos)    = pos
getPositionFromType (Fun pos _ _) = pos

getInner :: Checker TypeEnv
getInner = get >>= \s -> return $ fst s

getOuther :: Checker TypeEnv
getOuther = get >>= \s -> return $ snd s

putInner :: TypeEnv -> Checker ()
putInner inenv = getOuther >>= \outenv -> put (inenv, outenv)

putOuther :: TypeEnv -> Checker ()
putOuther outenv = getInner >>= \inenv -> put (inenv, outenv)

getVarType :: Name -> Checker (Maybe (Type Position))
getVarType name = do
  (inenv, outenv) <- get
  return $ case Map.lookup name inenv of
    Nothing -> Map.lookup name outenv
    e -> e

-- Top checker functions --

check :: Program Position -> IO ()
check program =
  case evalState (runExceptT (checkProgram program)) (Map.empty, Map.empty) of
    Left err -> print err >> exitFailure
    Right _ -> return ()

checkProgram :: Program Position -> Checker ()
checkProgram (Program _ topdefs) = do
  mapM_ addFnDecl inbuildFunctions
  mapM_ addFnDecl topdefs
  mapM_ checkTopDef topdefs

addFnDecl :: TopDef Position -> Checker ()
addFnDecl (FnDef fnpos rettype (Ident name) args _) = do
  decls <- getOuther
  case Map.lookup name decls of
    Just t -> throwError (Redefinition name fnpos (getPositionFromType t))
    Nothing -> let argtypes = map (\(Arg _ t _) -> t) args in
      putOuther $ Map.insert name (Fun fnpos rettype argtypes) decls

checkTopDef :: TopDef Position -> Checker ()
checkTopDef (FnDef _ ftype _ args block) = do
  mapM_ addArg args
  runReaderT (checkBlock block) ftype

addArg :: Arg Position -> Checker ()
addArg (Arg pos argtype (Ident name)) = do
  env <- getInner
  case Map.lookup name env of
    Just t  -> throwError (Redefinition name pos (getPositionFromType t))
    Nothing -> putInner $ Map.insert name argtype env

checkBlock :: Block Position -> StatementChecker ()
checkBlock (Block _ stmts) = mapM_ checkStmt stmts

-- Statements --

checkStmt :: Stmt Position -> StatementChecker ()

checkStmt (Empty _) = return ()

checkStmt (BStmt _ block) = do
  (inenv, outenv) <- get
  let outenv' = Map.union inenv outenv
  put (Map.empty, outenv')
  ret <- checkBlock block
  put (inenv, outenv)
  return ret

checkStmt (Decl pos (Void _) _) = throwError (InvalidDeclarationType pos)
checkStmt (Decl pos Fun{} _) = throwError (InvalidDeclarationType pos)
checkStmt (Decl _ argtype items) = mapM_ checkDecl items where
  checkDecl :: Item Position -> StatementChecker ()
  checkDecl (NoInit pos (Ident name)) = do
    inenv <- lift getInner
    case Map.lookup name inenv of
      Just t -> throwError (Redefinition name pos (getPositionFromType t))
      Nothing -> lift $ putInner (Map.insert name argtype inenv)
  checkDecl (Init pos ident@(Ident name) expr) = do
    exprtype <- checkExpr expr
    unless (exprtype == argtype) $ throwError (TypeMismatch name exprtype argtype)
    checkDecl (NoInit pos ident)

checkStmt (Ass pos ident expr) = do
  exprtype <- checkExpr expr
  checkVarMatch pos ident exprtype

checkStmt (Incr pos ident) = checkVarMatch pos ident (Int pos)

checkStmt (Decr pos ident) = checkVarMatch pos ident (Int pos)

checkStmt (VRet pos) = do
  rtype <- ask
  unless (rtype == Void pos) $ throwError (InvalidReturnType (fmap (const pos) rtype))

checkStmt (Ret pos expr) = do
  rtype <- ask
  etype <- checkExpr expr
  unless (rtype == etype) $ throwError (InvalidReturnType (fmap (const pos) rtype))

checkStmt (Cond pos expr stmt) = do
  exprtype <- checkExpr expr
  unless (exprtype == Bool pos) $ throwError (TypeMismatchAnonymous exprtype)
  checkStmt stmt

checkStmt (CondElse pos expr stmt1 stmt2) = do
  exprtype <- checkExpr expr
  unless (exprtype == Bool pos) $ throwError (TypeMismatchAnonymous exprtype)
  checkStmt stmt1
  checkStmt stmt2

checkStmt (While pos expr stmt) = do
  exprtype <- checkExpr expr
  unless (exprtype == Bool pos) $ throwError (TypeMismatchAnonymous exprtype)
  checkStmt stmt

checkStmt (SExp _ expr) = void $ checkExpr expr

checkVarMatch :: Position -> Ident -> Type Position -> StatementChecker ()
checkVarMatch pos (Ident name) acttype = do
  mvartype <- lift $ getVarType name
  when (isNothing mvartype) $ throwError (UndefinedVariable name pos)
  let vartype = fromJust mvartype
  unless (vartype == acttype) $ throwError (TypeMismatch name acttype vartype)

-- Expressions --

checkExpr :: Expr Position -> ExpressionChecker

checkExpr (EVar pos (Ident name)) = do
  (inenv, outenv) <- get
  case Map.lookup name inenv of
    Just t -> return t
    Nothing -> case Map.lookup name outenv of
      Just t -> return t
      Nothing -> throwError (UndefinedVariable name pos)

checkExpr (ELitTrue pos) = return $ Bool pos

checkExpr (ELitFalse pos) = return $ Bool pos

checkExpr (EApp pos (Ident name) exprs) = do
  exprtypes <- mapM checkExpr exprs
  mfuntype <- lift $ getVarType name
  when (isNothing mfuntype) $ throwError (UndefinedFunction name pos)
  case fromJust mfuntype of
    Fun _ rettype argtypes -> do
      unless (length exprtypes == length argtypes) $
        throwError (InvalidNumberOfArguments name pos)
      unless (and $ zipWith (==) exprtypes argtypes) $
        throwError (InvalidArgumentType name pos)
      return (fmap (const pos) rettype)
    _ -> throwError $ VariableCall name pos

checkExpr (ELitInt pos _) = return $ Int pos

checkExpr (EString pos _) = return $ Str pos

checkExpr (Neg pos expr) = checkUnOp expr (Int pos)

checkExpr (Not pos expr) = checkUnOp expr (Bool pos)

checkExpr (EAdd pos expr1 (Plus _) expr2) = do
  exprtype1 <- checkExpr expr1
  case exprtype1 of
    Int _ -> checkUnOp expr2 (Int pos)
    Str _ -> checkUnOp expr2 (Str pos)
    Bool _ -> throwError $ TypeMismatchAnonymous $ Bool pos
    _ -> throwError $ UnexpectedError pos

checkExpr (EAdd pos expr1 (Minus _) expr2) = checkBinOp expr1 expr2 (Int pos)

checkExpr (EMul pos expr1 _ expr2) = checkBinOp expr1 expr2 (Int pos)

checkExpr (ERel pos expr1 op expr2) = do
  exprtype1 <- checkExpr expr1
  exprtype2 <- checkExpr expr2
  let reltype = fmap (const pos) exprtype1
  unless (exprtype1 == exprtype2) $ throwError (TypeMismatchAnonymous reltype)
  case op of
    EQU _ -> return reltype
    NE _ -> return reltype
    _ -> if reltype == Bool pos
      then throwError (TypeMismatchAnonymous reltype)
      else return reltype

checkExpr (EAnd pos expr1 expr2) = checkBinOp expr1 expr2 (Bool pos)

checkExpr (EOr pos expr1 expr2) = checkBinOp expr1 expr2 (Bool pos)

checkUnOp :: Expr Position -> Type Position -> ExpressionChecker
checkUnOp expr optype= do
  exprtype <- checkExpr expr
  if exprtype == optype
    then return optype
    else throwError $ TypeMismatchAnonymous optype

checkBinOp :: Expr Position -> Expr Position -> Type Position -> ExpressionChecker
checkBinOp expr1 expr2 optype= do
  exprtype1 <- checkExpr expr1
  exprtype2 <- checkExpr expr2
  if exprtype1 == optype && exprtype2 == optype
    then return optype
    else throwError $ TypeMismatchAnonymous optype
