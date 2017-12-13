module StaticAnalysis where

import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AbsLatte

type Name = String
type TypeEnv = Map.Map Name (Type Position)
type TypeScope = (TypeEnv, TypeEnv)
type Position = Maybe (Int, Int)
data Error
  = Redefinition Name Position Position
  | TypeMismatch (Type Position)
  | NoReturn Name Position
  | InvalidReturnType Position
  | UnexpectedError Position
  | UndefinedVariable Name Position
  | UndefinedFunction Name Position
  | VariableCall Name Position
  | InvalidNumberOfArguments Name Position
  | InvalidArgumentType Name Position
type Checker a = ExceptT Error (State TypeScope) a
type StatementChecker a = ReaderT (Type Position) (ExceptT Error (State TypeScope)) a
type ExpressionChecker = StatementChecker (Type Position)
-- type StatementChecker a = ReaderT (Type ()) (ExceptT Error (State TypeScope)) a
-- type ExpressionChecker = StatementChecker (Type ())
-- type StatementChecker = Checker (Maybe (Type Position))

-- instance Eq Type where
--   x == y = True

-- instance Show a => Show (Program a) where
--   show (Program a t) = "ala"

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

analise :: Program Position -> IO ()
analise p = print p
-- analise p = print ((Str (Just (0,0))) == (Str (Just (1,1))))

checkProgram :: Program Position -> Checker ()
checkProgram (Program pos topdefs) = do
  mapM_ addFnDecl inbuildFunctions
  mapM_ addFnDecl topdefs
  mapM_ checkTopDef topdefs

-- addFnDecl :: TypeEnv -> TopDef Position -> TypeEnv
-- addFnDecl decls (FnDef fnpos rettype (Ident name) args _) =
--   let argtypes = map (\(Arg _ t _) -> t) args in
--   Map.insert name (Fun fnpos rettype argtypes) decls

addFnDecl :: TopDef Position -> Checker ()
addFnDecl (FnDef fnpos rettype (Ident name) args _) = do
  decls <- getOuther
  case Map.lookup name decls of
    Just t -> throwError (Redefinition name fnpos (getPositionFromType t))
    Nothing -> let argtypes = map (\(Arg _ t _) -> t) args in
      putOuther $ Map.insert name (Fun fnpos rettype argtypes) decls

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

checkTopDef :: TopDef Position -> Checker ()
checkTopDef (FnDef fpos ftype (Ident name) args block) = do
  mapM_ addArg args
  ret <- runReaderT (checkBlock block) ftype--(void ftype)
  unless ret $ throwError (NoReturn name fpos) -- void nie musi mieć return

addArg :: Arg Position -> Checker ()
addArg (Arg pos argtype (Ident name)) = do
  env <- getInner
  case Map.lookup name env of
    Just t  -> throwError (Redefinition name pos (getPositionFromType t))
    Nothing -> putInner $ Map.insert name argtype env

-- Bool oznacza czy wystąpił return czy nie
checkBlock :: Block Position -> StatementChecker Bool
checkBlock (Block _ stmts) = or <$> mapM checkStmt stmts

-- Statements --

checkStmt :: Stmt Position -> StatementChecker Bool

checkStmt (Empty _) = return False

checkStmt (BStmt _ block) = do
  (inenv, outenv) <- get
  let outenv' = Map.union inenv outenv
  put (Map.empty, outenv')
  ret <- checkBlock block
  put (inenv, outenv)
  return ret

checkStmt (VRet pos) = do
  rtype <- ask
  if rtype == Void pos then return True else throwError (InvalidReturnType pos)
--if rtype == Void () then return True else throwError (InvalidReturnType pos)

checkStmt (Ret pos expr) = do
  rtype <- ask
  etype <- checkExpr expr
  if rtype == etype then return True else throwError (InvalidReturnType pos)

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
  (inenv, outenv) <- get
  let funtype = case Map.lookup name inenv of
        Nothing -> Map.lookup name outenv
        e -> e
  when (isNothing funtype) $ throwError (UndefinedFunction name pos)
  case fromJust funtype of
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
    Bool _ -> throwError $ TypeMismatch $ Bool pos
    _ -> throwError $ UnexpectedError pos

checkExpr (EAdd pos expr1 (Minus _) expr2) = checkBinOp expr1 expr2 (Int pos)

checkExpr (EMul pos expr1 _ expr2) = checkBinOp expr1 expr2 (Int pos)

checkExpr (ERel pos expr1 op expr2) = do
  exprtype1 <- checkExpr expr1
  exprtype2 <- checkExpr expr2
  let reltype = fmap (const pos) exprtype1
  unless (exprtype1 == exprtype2) $ throwError (TypeMismatch reltype)
  case op of
    EQU _ -> return reltype
    NE _ -> return reltype
    _ -> if reltype == Bool pos
      then throwError (TypeMismatch reltype)
      else return reltype

checkExpr (EAnd pos expr1 expr2) = checkBinOp expr1 expr2 (Bool pos)

checkExpr (EOr pos expr1 expr2) = checkBinOp expr1 expr2 (Bool pos)

checkUnOp :: Expr Position -> Type Position -> ExpressionChecker
checkUnOp expr optype= do
  exprtype <- checkExpr expr
  if exprtype == optype
    then return optype
    else throwError $ TypeMismatch optype

checkBinOp :: Expr Position -> Expr Position -> Type Position -> ExpressionChecker
checkBinOp expr1 expr2 optype= do
  exprtype1 <- checkExpr expr1
  exprtype2 <- checkExpr expr2
  if exprtype1 == optype && exprtype2 == optype
    then return optype
    else throwError $ TypeMismatch optype
