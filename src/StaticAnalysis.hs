module StaticAnalysis where

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsLatte

type Name = String
type TypeEnv = Map.Map Name (Type Position)
type TypeScope = (TypeEnv, TypeEnv)
type Position = Maybe (Int, Int)
data Error
  = Redefinition Name Position Position
  | NoReturn Name Position
  | InvalidReturn Position
type Checker a = ExceptT Error (State TypeScope) a
type StatementChecker a = ReaderT (Type ()) (ExceptT Error (State TypeScope)) a
type ExpressionChecker = StatementChecker (Type ())
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
getPositionFromType (Int pos) = pos
getPositionFromType (Str pos) = pos
getPositionFromType (Bool pos) = pos
getPositionFromType (Void pos) = pos
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
  ret <- runReaderT (checkBlock block) (void ftype)
  unless ret $ throwError (NoReturn name fpos)

addArg :: Arg Position -> Checker ()
addArg (Arg pos argtype (Ident name)) = do
  env <- getInner
  case Map.lookup name env of
    Just t -> throwError (Redefinition name pos (getPositionFromType t))
    Nothing -> putInner $ Map.insert name argtype env

checkBlock :: Block Position -> StatementChecker Bool
checkBlock (Block _ stmts) = or <$> mapM checkStmt stmts

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
  if rtype == Void () then return True else throwError (InvalidReturn pos)

checkStmt (Ret pos expr) = do
  rtype <- ask
  etype <- checkExpr expr
  if rtype == etype then return True else throwError (InvalidReturn pos)

checkExpr :: Expr Position -> ExpressionChecker
checkExpr (ELitTrue pos) = return (Void ())
