module ExpressionEvaluation where

-- Imports --

import Globals (Position)
import AbsLatte

-- Top functions --

evalProgram :: Program Position -> Program Position
evalProgram (Program pos topdefs) = Program pos (map evalTopDef topdefs)

evalTopDef :: TopDef Position -> TopDef Position
evalTopDef (FnDef pos ret ident args block) =
  FnDef pos ret ident args (evalBlock block)

evalBlock :: Block Position -> Block Position
evalBlock (Block pos stmts) =
  Block pos (filter (isStmtEmpty . evalStmt) stmts) where
  isStmtEmpty (Empty _) = False
  isStmtEmpty _ = True

-- Statements --

evalStmt :: Stmt Position -> Stmt Position

evalStmt s@(BStmt pos block) =
  case evalBlock block of
    Block _ [] -> Empty pos
    _ -> s

evalStmt (Decl pos argtype items) = Decl pos argtype (map evalItem items) where
  evalItem (Init ipos ident expr) = Init ipos ident (evalExpr expr)
  evalItem i = i

evalStmt (Ass pos ident expr) = Ass pos ident (evalExpr expr)

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

evalStmt (SExp pos expr) = SExp pos (evalExpr expr)

evalStmt s = s

-- Expressions --

evalExpr :: Expr Position -> Expr Position

evalExpr (EApp pos ident exprs) = EApp pos ident (map evalExpr exprs)

evalExpr e@(Neg pos expr) =
  case evalExpr expr of
    ELitTrue _ -> ELitFalse pos
    ELitFalse _ -> ELitTrue pos
    _ -> e

evalExpr e@(Not pos expr) =
  case evalExpr expr of
    ELitInt _ n -> ELitInt pos (-n)
    _ -> e

evalExpr e@(EAdd pos expr1 (Plus _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 + n2)
    (EString _ s1, EString _ s2) -> EString pos (s1 ++ s2)
    _ -> e

evalExpr e@(EAdd pos expr1 (Minus _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 - n2)
    _ -> e

evalExpr e@(EMul pos expr1 (Times _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 * n2)
    _ -> e

evalExpr e@(EMul pos expr1 (Div _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ _, ELitInt _ 0) -> e
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 `quot` n2)
    _ -> e

evalExpr e@(EMul pos expr1 (Mod _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ _, ELitInt _ 0) -> e
    (ELitInt _ n1, ELitInt _ n2) -> ELitInt pos (n1 `mod` n2)
    _ -> e

evalExpr e@(ERel pos expr1 (EQU _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    (ELitFalse _, ELitFalse _) -> ELitTrue pos
    (ELitFalse _, ELitTrue _) -> ELitFalse pos
    (ELitTrue _, ELitFalse _) -> ELitFalse pos
    (ELitInt _ n1, ELitInt _ n2) -> emitBool (n1 == n2) pos
    (EString _ s1, EString _ s2) -> emitBool (s1 == s2) pos
    _ -> e

evalExpr e@(ERel pos expr1 (NE _) expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitFalse pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitTrue pos
    (ELitTrue _, ELitFalse _) -> ELitTrue pos
    (ELitInt _ n1, ELitInt _ n2) -> emitBool (n1 /= n2) pos
    (EString _ s1, EString _ s2) -> emitBool (s1 /= s2) pos
    _ -> e

evalExpr e@(ERel pos expr1 op expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitInt _ n1, ELitInt _ n2) -> case op of
      LTH _ -> emitBool (n1 < n2) pos
      LE _ -> emitBool (n1 <= n2) pos
      GTH _ -> emitBool (n1 > n2) pos
      GE _ -> emitBool (n1 >= n2) pos
      _ -> e
    _ -> e

evalExpr e@(EAnd pos expr1 expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitFalse pos
    (ELitTrue _, ELitFalse _) -> ELitFalse pos
    _ -> e

evalExpr e@(EOr pos expr1 expr2) =
  case (evalExpr expr1, evalExpr expr2) of
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitTrue pos
    (ELitTrue _, ELitFalse _) -> ELitTrue pos
    _ -> e

evalExpr e = e

emitBool :: Bool -> Position -> Expr Position
emitBool b pos = if b then ELitTrue pos else ELitFalse pos
