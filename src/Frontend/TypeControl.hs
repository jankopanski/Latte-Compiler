module Frontend.TypeControl where -- abstract syntax tree checker

-- Imports --

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)

import Parser.AbsLatte
import Frontend.Globals

-- Data structures --

type Name = String
type ClsEnv = Map.Map Ident (Map.Map Ident (Type Position))
type TypeEnv = Map.Map Name (Type Position)
type TypeScope = (TypeEnv, TypeEnv)

data Error
  = Redefinition Name Position Position
  | TypeMismatch Name (Type Position) (Type Position)
  | TypeMismatchAnonymous (Type Position)
  | UndefinedType Name Position
  | UndefinedVariable Name Position
  | UndefinedFunction Name Position
  | VariableCall Name Position
  | InvalidNumberOfArguments Name Position
  | InvalidArgumentType Name Position
  | InvalidDeclarationType Position
  | InvalidReturnType (Type Position)
  | CustomError String
  | CustomErrorPosition String Position
  | UnexpectedError Position
instance Show Error where
  show (Redefinition name new old) =
    showErrorPosition new ++ "Redefinition of '" ++ name ++
      "'\n\tPreviously defined at " ++ showPosition old
  show (TypeMismatch name actual expected) =
    showErrorPosition (getPositionFromType actual) ++ "Type mismatch, used type " ++
      show actual ++ ", expected " ++ show expected ++ ", '" ++ name  ++
      "' was declared at " ++ showPosition (getPositionFromType expected)
  show (TypeMismatchAnonymous t) =
    showErrorPosition (getPositionFromType t) ++ "Expected type " ++ show t
  show (UndefinedType name pos) =
    showErrorPosition pos ++ "Undefined type '" ++ name ++ "'"
  show (UndefinedFunction name pos) =
    showErrorPosition pos ++ "Undefined function '" ++ name ++ "'"
  show (UndefinedVariable name pos) =
    showErrorPosition pos ++ "Undefined variable '" ++ name ++ "'"
  show (VariableCall name pos) =
    showErrorPosition pos ++ "'" ++ name ++ "' is not callable"
  show (InvalidNumberOfArguments name pos) =
    showErrorPosition pos ++ "Invalid number of arguments in call to function '"
    ++ name ++ "'"
  show (InvalidArgumentType name pos) =
    showErrorPosition pos ++ "Invalid type of argument in call to function '"
    ++ name ++ "'"
  show (InvalidDeclarationType pos) =
    showErrorPosition pos ++ "Invalid declaration type"
  show (InvalidReturnType t) =
    showErrorPosition (getPositionFromType t) ++ "Invalid type of return, expected " ++
    show t
  show (CustomError s) = s
  show (CustomErrorPosition s pos) = showErrorPosition pos ++ s
  show (UnexpectedError pos) =
    showErrorPosition pos ++ "Unexpected error in type control module"

type Checker a = ExceptT Error (State TypeScope) a
type StatementChecker a = ReaderT (ClsEnv, Type Position) (ExceptT Error (State TypeScope)) a
type ExpressionChecker = StatementChecker (Type Position)

-- Helper functions --

getPositionFromType :: Type Position -> Position
getPositionFromType (Int pos)     = pos
getPositionFromType (Str pos)     = pos
getPositionFromType (Bool pos)    = pos
getPositionFromType (Void pos)    = pos
getPositionFromType (Arr pos _)   = pos
getPositionFromType (Cls pos _)   = pos
getPositionFromType (Fun pos _ _) = pos

getInner :: Checker TypeEnv
getInner = get >>= \s -> return $ fst s

getOuther :: Checker TypeEnv
getOuther = get >>= \s -> return $ snd s

putInner :: TypeEnv -> Checker ()
putInner inenv = getOuther >>= \outenv -> put (inenv, outenv)

putOuther :: TypeEnv -> Checker ()
putOuther outenv = getInner >>= \inenv -> put (inenv, outenv)

getClasses :: StatementChecker ClsEnv
getClasses = ask >>= \env -> return $ fst env

getVarType :: Name -> Checker (Maybe (Type Position))
getVarType name = do
  (inenv, outenv) <- get
  return $ case Map.lookup name inenv of
    Nothing -> Map.lookup name outenv
    e -> e

getFinalType :: Position -> Ident -> [Var Position] -> ExpressionChecker
getFinalType pos ident@(Ident name) vars = do
  mclstype <- lift $ getVarType name
  when (isNothing mclstype) $ throwError (UndefinedVariable name pos)
  let vartype = fromJust mclstype
  unless (vartype == Cls pos ident) $ throwError (TypeMismatch name (Cls pos ident) vartype)
  foldM resolveVar (Cls pos ident) vars
  where
  resolveVar :: Type Position -> Var Position -> StatementChecker (Type Position)
  resolveVar (Cls pos' clsid@(Ident name')) (Var pos'' fieldid@(Ident name'')) = do
    clsenv <- getClasses
    case Map.lookup clsid clsenv of
      Nothing -> throwError (UndefinedType name' pos')
      Just fieldMap -> case Map.lookup fieldid fieldMap of
        Nothing -> throwError (UndefinedVariable name'' pos'')
        Just t -> return t
  resolveVar t (Var pos' ident'@(Ident name')) =
    throwError (TypeMismatch name' (Cls pos' ident') t)

-- Top functions --

runTypeControl :: Program Position -> IO ()
runTypeControl program =
  case evalState (runExceptT (checkProgram program)) (Map.empty, Map.empty) of
    Left err -> printError err
    Right _ -> return ()

checkProgram :: Program Position -> Checker ()
checkProgram (Program _ topdefs) = do
  clsenv <- collectClassDefs topdefs
  checkClassDefs clsenv
  mapM_ addFnDecl inbuildFunctions
  mapM_ addFnDecl topdefs
  checkIsMain
  mapM_ (checkFunction clsenv) topdefs

collectClassDefs :: [TopDef Position] -> Checker ClsEnv
collectClassDefs topdefs = do
  clsdefs <- foldM addClassDef Map.empty topdefs
  return $ Map.map fst clsdefs

addClassDef :: Map.Map Ident (Map.Map Ident (Type Position), Position) -> TopDef Position ->
  Checker (Map.Map Ident (Map.Map Ident (Type Position), Position))
addClassDef clsdefs (ClsDef pos ident@(Ident name) fields) =
  case Map.lookup ident clsdefs of
    Just (_, pos') -> throwError (Redefinition name pos pos')
    Nothing -> foldM addField Map.empty fields >>=
      \fieldMap -> return $ Map.insert ident (fieldMap, pos) clsdefs
addClassDef clsdefs _ = return clsdefs

addField :: Map.Map Ident (Type Position) -> Field Position ->
  Checker (Map.Map Ident (Type Position))
addField fieldMap (Field pos t ident@(Ident name)) =
  case Map.lookup ident fieldMap of
    Just t' -> throwError (Redefinition name pos (getPositionFromType t'))
    Nothing -> return $ Map.insert ident t fieldMap

checkClassDefs :: ClsEnv -> Checker ()
checkClassDefs clsdefs = mapM_ checkClassFields clsdefs where
  checkClassFields :: Map.Map Ident (Type Position) -> Checker ()
  checkClassFields = mapM_ checkField
  checkField :: Type Position -> Checker ()
  checkField t@Fun{} = throwError (TypeMismatchAnonymous t)
  checkField t@Void{} = throwError (TypeMismatchAnonymous t)
  checkField (Cls pos ident@(Ident name)) = case Map.lookup ident clsdefs of
    Nothing -> throwError (UndefinedType name pos)
    Just _ -> return ()
  checkField _ = return ()

addFnDecl :: TopDef Position -> Checker ()
addFnDecl (FnDef fnpos rettype (Ident name) args _) = do
  decls <- getOuther
  case Map.lookup name decls of
    Just t -> throwError (Redefinition name fnpos (getPositionFromType t))
    Nothing -> do
      let argtypes = map (\(Arg _ t _) -> t) args
      when (Void fnpos `elem` argtypes) $
        throwError (CustomErrorPosition "Void argument" fnpos)
      putOuther $ Map.insert name (Fun fnpos rettype argtypes) decls
addFnDecl ClsDef{} = return ()

checkIsMain :: Checker ()
checkIsMain = do
  decls <- getOuther
  case Map.lookup "main" decls of
    Nothing -> throwError (CustomError "No 'main' function")
    Just fun -> case fun of
      (Fun _ (Int _) []) -> return ()
      (Fun pos _ _) ->
        throwError (CustomErrorPosition "Invalid 'main' declaration" pos)
      t -> throwError (TypeMismatchAnonymous t)

checkFunction :: ClsEnv -> TopDef Position -> Checker ()
checkFunction clsenv (FnDef _ ftype _ args block) = do
  envs <- get
  mapM_ addArg args
  runReaderT (checkBlock block) (clsenv, ftype)
  put envs
checkFunction _ _ = return ()

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

checkStmt (Ass _ [Var pos ident] expr) = do
  exprtype <- checkExpr expr
  checkVarMatch pos ident exprtype

checkStmt (Ass _ (Var pos ident@(Ident name) : vars) expr) = do
  exprtype <- checkExpr expr
  finaltype <- getFinalType pos ident vars
  unless (finaltype == exprtype) $ throwError (TypeMismatch name finaltype exprtype)

checkStmt (Incr pos ident) = checkVarMatch pos ident (Int pos)

checkStmt (Decr pos ident) = checkVarMatch pos ident (Int pos)

checkStmt (VRet pos) = do
  rtype <- fmap snd ask
  unless (rtype == Void pos) $ throwError (InvalidReturnType (fmap (const pos) rtype))

checkStmt (Ret pos expr) = do
  rtype <- fmap snd ask
  etype <- checkExpr expr
  unless (rtype == etype && rtype /= Void pos) $
    throwError (InvalidReturnType (fmap (const pos) rtype))

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

checkExpr (EVar _ [Var pos (Ident name)]) = do
  (inenv, outenv) <- get
  case Map.lookup name inenv of
    Just t -> return t
    Nothing -> case Map.lookup name outenv of
      Just t -> return t
      Nothing -> throwError (UndefinedVariable name pos)

checkExpr (EVar _ (Var pos ident : vars)) = getFinalType pos ident vars

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
  let reltype = Bool pos
  unless (exprtype1 == exprtype2) $ throwError
    (TypeMismatchAnonymous $ fmap (const $ getPositionFromType exprtype2) exprtype1)
  case op of
    EQU _ -> return reltype
    NE _ -> return reltype
    _ -> if exprtype1 == Int pos
      then return reltype
      else throwError (TypeMismatchAnonymous $ Int pos)

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
