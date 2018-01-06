module Frontend.ReturnEvaluation where

-- Imports --

import Parser.AbsLatte
import Frontend.Globals

-- Data structures --

runReturnEvaluation :: Program Position -> IO (Program Position)
runReturnEvaluation = returnEvalProgram

returnEvalProgram :: Program Position -> IO (Program Position)
returnEvalProgram (Program pos topdefs) =
  case mapM returnEvalTopDef topdefs of
    Right topdefs' -> return $ Program pos topdefs'
    Left (Ident name, errpos) ->
      putError $ showErrorPosition errpos ++
        "No return statement in function '" ++ name ++ "'"

returnEvalTopDef :: TopDef Position -> Either (Ident, Position) (TopDef Position)
returnEvalTopDef (FnDef pos ret ident args block) =
  let (block', wasReturn) = returnEvalBlock block in
  case (ret, wasReturn) of
    (Void _, _) -> Right $ FnDef pos ret ident args block'
    (_, False) -> Left (ident, pos)
    _ -> Right $ FnDef pos ret ident args block'

returnEvalBlock :: Block Position -> (Block Position, Bool)
returnEvalBlock (Block pos stmts) =
  let (stmts', wasReturn) = takeWhileEvalStmt stmts [] in
  (Block pos stmts', wasReturn) where
    takeWhileEvalStmt :: [Stmt Position] -> [Stmt Position] -> ([Stmt Position], Bool)
    takeWhileEvalStmt [] l = (reverse l, False)
    takeWhileEvalStmt (h:t) l = let (s, b) = returnEvalStmt h in
      if b then (reverse (s:l), True) else takeWhileEvalStmt t (s:l)

returnEvalStmt :: Stmt Position -> (Stmt Position, Bool)

returnEvalStmt (BStmt pos block) =
  let (block', b) = returnEvalBlock block in
  (BStmt pos block', b)

returnEvalStmt s@VRet{} = (s, True)

returnEvalStmt s@Ret{} = (s, True)

returnEvalStmt (Cond pos expr stmt) =
  let (stmt', _) = returnEvalStmt stmt in
  (Cond pos expr stmt', False)

returnEvalStmt (CondElse pos expr stmt1 stmt2) =
  let (stmt1', b1) = returnEvalStmt stmt1 in
  let (stmt2', b2) = returnEvalStmt stmt2 in
  if b1 && b2
    then (CondElse pos expr stmt1' stmt2', True)
    else (CondElse pos expr stmt1' stmt2', False)

returnEvalStmt (While pos expr stmt) =
  let (stmt', _) = returnEvalStmt stmt in
  (While pos expr stmt', False)

returnEvalStmt s = (s, False)
