module Frontend.StaticEvaluation where

-- Imports --

import Frontend.Globals (Position)
import Parser.AbsLatte

-- Top functions --

runStaticEvaluation :: Program Position -> IO (Program Position)
runStaticEvaluation = return . evalProgram

evalProgram :: Program Position -> Program Position
evalProgram (Program pos topdefs) = Program pos (map evalTopDef topdefs)

evalTopDef :: TopDef Position -> TopDef Position
evalTopDef (FnDef pos ret ident args block) =
  FnDef pos ret ident args (evalBlock block)

evalBlock :: Block Position -> Block Position
evalBlock (Block pos stmts) =
  let stmts' = map evalStmt stmts in
  Block pos (filter isStmtEmpty stmts') where
    isStmtEmpty (Empty _) = False
    isStmtEmpty _ = True

-- Statements --

evalStmt :: Stmt Position -> Stmt Position

evalStmt (BStmt pos block) =
  let block' = evalBlock block in
  case block' of
    Block _ [] -> Empty pos
    _ -> BStmt pos block'

evalStmt (Decl pos argtype items) = Decl pos argtype (map evalItem items) where
  evalItem (Init ipos ident expr) = Init ipos ident (evalExpr expr)
  evalItem i = i

evalStmt (Ass pos ident expr) = Ass pos ident (evalExpr expr)

evalStmt (ArrAss pos ident expr1 expr2) = ArrAss pos ident (evalExpr expr1) (evalExpr expr2)

evalStmt (Ret pos expr) = Ret pos (evalExpr expr)

evalStmt (Cond pos expr stmt) =
  let stmt' = evalStmt stmt in
  let expr' = evalExpr expr in
  case evalExpr expr of
    ELitTrue _ -> stmt'
    ELitFalse _ -> Empty pos
    _ -> Cond pos expr' stmt'

evalStmt (CondElse pos expr stmt1 stmt2) =
  let stmt1' = evalStmt stmt1 in
  let stmt2' = evalStmt stmt2 in
  let expr' = evalExpr expr in
  case evalExpr expr of
    ELitTrue _ -> stmt1'
    ELitFalse _ -> stmt2'
    _ -> CondElse pos expr' stmt1' stmt2'

evalStmt (While pos expr stmt) = While pos (evalExpr expr) (evalStmt stmt)

evalStmt (For pos t ident1 ident2 stmt) = For pos t ident1 ident2 (evalStmt stmt)

evalStmt (SExp pos expr) = SExp pos (evalExpr expr)

evalStmt s = s

-- Expressions --

evalExpr :: Expr Position -> Expr Position

evalExpr (EApp pos ident exprs) = EApp pos ident (map evalExpr exprs)

evalExpr (EString pos s) = EString pos (read s)

evalExpr (ENewArr pos t expr) = ENewArr pos t (evalExpr expr)

evalExpr (EAccArr pos ident expr) = EAccArr pos ident (evalExpr expr)

evalExpr (Neg pos expr) =
  case evalExpr expr of
    ELitInt _ n -> ELitInt pos (-n)
    _ -> Neg pos $ evalExpr expr

evalExpr (Not pos expr) =
  case evalExpr expr of
    ELitTrue _ -> ELitFalse pos
    ELitFalse _ -> ELitTrue pos
    _ -> Not pos $ evalExpr expr

evalExpr (EAdd pos expr1 op@(Plus _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 + n2)
    (EString _ s1, EString _ s2) -> EString pos (s1 ++ s2)
    _ -> EAdd pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (EAdd pos expr1 op@(Minus _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 - n2)
    _ -> EAdd pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (EMul pos expr1 op@(Times _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 * n2)
    _ -> EMul pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (EMul pos expr1 op@(Div _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ _, ELitInt _ 0) -> EMul pos (evalExpr expr1) op (evalExpr expr2)
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 `quot` n2)
    _ -> EMul pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (EMul pos expr1 op@(Mod _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ _, ELitInt _ 0) -> EMul pos (evalExpr expr1) op (evalExpr expr2)
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 `rem` n2)
    _ -> EMul pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (ERel pos expr1 op@(EQU _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    (ELitFalse _, ELitFalse _) -> ELitTrue pos
    (ELitFalse _, ELitTrue _) -> ELitFalse pos
    (ELitTrue _, ELitFalse _) -> ELitFalse pos
    (ELitInt _ n1, ELitInt _ n2) -> emitBool (n1 == n2) pos
    (EString _ s1, EString _ s2) -> emitBool (s1 == s2) pos
    _ -> ERel pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (ERel pos expr1 op@(NE _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitFalse pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitTrue pos
    (ELitTrue _, ELitFalse _) -> ELitTrue pos
    (ELitInt _ n1, ELitInt _ n2) -> emitBool (n1 /= n2) pos
    (EString _ s1, EString _ s2) -> emitBool (s1 /= s2) pos
    _ -> ERel pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (ERel pos expr1 op expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> case op of
      LTH _ -> emitBool (n1 < n2) pos
      LE _ -> emitBool (n1 <= n2) pos
      GTH _ -> emitBool (n1 > n2) pos
      GE _ -> emitBool (n1 >= n2) pos
      _ -> ERel pos (evalExpr expr1) op (evalExpr expr2)
    _ -> ERel pos (evalExpr expr1) op (evalExpr expr2)

evalExpr (EAnd pos expr1 expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitFalse _, _) -> ELitFalse pos
    (_, ELitTrue _) -> evalExpr expr1
    (ELitTrue _, _) -> evalExpr expr2
    _ -> EAnd pos (evalExpr expr1) (evalExpr expr2)

evalExpr (EOr pos expr1 expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, _) -> ELitTrue pos
    (_, ELitFalse _) -> evalExpr expr1
    (ELitFalse _, _) -> evalExpr expr2
    _ -> EOr pos (evalExpr expr1) (evalExpr expr2)

evalExpr e = e

emitBool :: Bool -> Position -> Expr Position
emitBool b pos = if b then ELitTrue pos else ELitFalse pos
