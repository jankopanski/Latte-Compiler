module Backend.IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Prelude hiding (lookup)
import Data.Map (Map, empty, (!), lookup, insert, assocs)

import Parser.AbsLatte
import Backend.IntermediateCode

-- Data structures --

newtype Environment = Environment {
  returnLabel :: Label
}

data Store = Store {
  labelCounter :: Integer,
  localSize :: Size,
  maxLocalSize :: Size,
  variableEnv :: Map Ident Memory,
  stringCounter :: Integer,
  stringEnv :: Map String Label
}

type TopGeneratorT = StateT Store IO
type TopGenerator a = TopGeneratorT a
type Generator a =  ReaderT Environment (WriterT [Instruction] TopGeneratorT) a
type StatementGenerator = Generator ()
type ExpressionGenerator = Generator (Operand, [Instruction])

-- Helper functions --

emptyStore :: Store
emptyStore = Store {
  labelCounter = 0,
  localSize = 0,
  maxLocalSize = 0,
  variableEnv = empty,
  stringCounter = 0,
  stringEnv = empty
}

sizeOf :: Type () -> Size
sizeOf _ = wordLen

defaultValue :: Type () -> Integer
defaultValue _ = 0

nextReg :: Register -> Register
nextReg = succ

toReg :: Operand -> Register
toReg (Reg r) = r
toReg _ = EAX

isCommutative :: BinaryOperator -> Bool
isCommutative ADD = True
isCommutative MUL = True
isCommutative _ = False

generr :: Show a => a -> Generator b
generr err = error $ "Unexpected error\n" ++ show err

newLabel :: TopGenerator Label
newLabel = state (\s -> let n = labelCounter s in (Label n, s {labelCounter = n + 1}))

getStringLabel :: String -> Generator Label
getStringLabel str = get >>= \store ->
  case lookup str (stringEnv store) of
    Just l -> return l
    Nothing -> do
      let n = stringCounter store
          l = StringLabel n
      put (store {stringCounter = n + 1, stringEnv = insert str l (stringEnv store)})
      return l

newLoc :: Type () -> Generator Memory
newLoc vartype = state (\s -> let size = localSize s in
  (MemoryLocal (size + wordLen), s {localSize = size + sizeOf vartype}))

getLoc :: Ident -> Generator Memory
getLoc ident = get >>= \s -> return $ variableEnv s ! ident

-- Top functions --

runIntermediateCodeGeneration :: Program () -> IO Code
runIntermediateCodeGeneration program = evalStateT (genProgram program) emptyStore

genProgram :: Program () -> TopGenerator Code
genProgram (Program () topdefs) = do
  fs <- mapM genTopDef topdefs
  store <- get
  let strs = assocs (stringEnv store)
  return (Code fs strs)

genTopDef :: TopDef () -> TopGenerator (Ident, [Instruction])
genTopDef (FnDef () _ ident args block) = do
  let env = getArgEnv args
  l <- newLabel
  s1 <- get
  put (s1 {localSize = 0, variableEnv = env})
  ins <- execWriterT (runReaderT (genBlock block) (Environment l))
  s2 <- get
  let callieSave = usedCallieSave ins
      fullIns = genProlog (maxLocalSize s2) callieSave ++ ins ++ genEpilog l callieSave
  return (ident, fullIns)

getArgEnv :: [Arg ()] -> Map Ident Memory
getArgEnv args = fst $ foldl addArg (empty, 2 * wordLen) args where
  addArg (env, size) (Arg () _ argid) =
    (insert argid (MemoryArgument size) env, size + wordLen)

usedCallieSave :: [Instruction] -> Register
usedCallieSave = foldl scanIns EAX where
  scanIns mr (IMov _ r) = max mr r
  scanIns mr _ = mr

genProlog :: Size -> Register -> [Instruction]
genProlog size reg = [IPush EBP, IMov (Reg ESP) EBP, IBinOp SUB ESP (Imm size)] ++
  case reg of
    EBX -> [IPush EBX]
    EDI -> [IPush EBX, IPush EDI]
    ESI -> [IPush EBX, IPush EDI, IPush ESI]
    _ -> []

genEpilog :: Label -> Register -> [Instruction]
genEpilog lab reg = [ILabel lab] ++ pops ++ [IMov (Reg EBP) ESP, IPop EBP, IRet] where
  pops = case reg of
    EBX -> [IPop EBX]
    EDI -> [IPop EDI, IPop EBX]
    ESI -> [IPop ESI, IPop EDI, IPop EBX]
    _ -> []

genBlock :: Block () -> StatementGenerator
genBlock (Block () stmts) = do
  s1 <- get
  mapM_ genStmt stmts
  s2 <- get
  put (s2 { localSize = localSize s1,
            maxLocalSize = max (maxLocalSize s2) (localSize s2),
            variableEnv = variableEnv s1 })

-- Statements --

genStmt :: Stmt () -> StatementGenerator

genStmt (Empty ()) = return ()

genStmt (BStmt () block) = genBlock block

genStmt (Decl () vartype items) = mapM_ genDecl items where
  genDecl :: Item () -> StatementGenerator
  genDecl (NoInit () ident) = do
    m <- newLoc vartype
    s <- get
    put (s {variableEnv = insert ident m (variableEnv s)})
    tell [IStore (Imm (defaultValue vartype)) m]
  genDecl (Init () ident expr) = do
    (o, i) <- genExpr expr
    m <- newLoc vartype
    s <- get
    put (s {variableEnv = insert ident m (variableEnv s)})
    tell $ i ++ [IStore o m]

genStmt (Ass () ident expr) = do
  (o, i) <- genExpr expr
  m2 <- getLoc ident
  case o of
    Mem m1 -> tell [ILoad m1 EAX, IStore (Reg EAX) m2]
    _ -> tell $ i ++ [IStore o m2]

genStmt (Incr () ident) = getLoc ident >>= \m -> tell [IUnOp INC (Mem m)]

genStmt (Decr () ident) = getLoc ident >>= \m -> tell [IUnOp DEC (Mem m)]

genStmt (Ret () expr) = do
  (o, i) <- genExpr expr
  env <- ask
  tell $ i ++ [IMov o EAX, IJump (returnLabel env)]

genStmt (VRet ()) = ask >>= \env -> tell [IJump (returnLabel env)]

genStmt (Cond () expr stmt) = do
  lthen <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  i <- genCond expr lthen lafter
  tell (i ++ [ILabel lthen])
  genStmt stmt
  tell [ILabel lafter]

genStmt (CondElse () expr stmt1 stmt2) = do
  lthen <- lift $ lift newLabel
  lelse <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  i <- genCond expr lthen lelse
  tell (i ++ [ILabel lthen])
  genStmt stmt1
  tell [IJump lafter, ILabel lelse]
  genStmt stmt2
  tell [ILabel lafter]

genStmt (While () expr stmt) = do
  lcond <- lift $ lift newLabel
  lloop <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  i <- genCond expr lloop lafter
  tell ([ILabel lcond] ++ i ++ [ILabel lloop])
  genStmt stmt
  tell [IJump lcond, ILabel lafter]

genStmt (SExp () expr) = genExpr expr >>= \(_, i) -> tell i

-- Expressions --

genExpr :: Expr () -> ExpressionGenerator

genExpr (EVar () ident) = getLoc ident >>= \m -> return (Mem m, [])

genExpr (ELitInt () n) = return (Imm n, [])

genExpr (ELitTrue ()) = return (Imm 1, [])

genExpr (ELitFalse ()) = return (Imm 0, [])

genExpr (EApp () ident exprs) = do
  params <- mapM genExpr exprs
  let ins = foldl (\ins' (o, i) -> i ++ [IParam o] ++ ins') [] params
  let reg = if null params then EAX else maximum $ map (toReg . fst) params
  return (Reg reg, ins ++ [ICall ident (fromIntegral $ length params), IMov (Reg EAX) reg])

genExpr (EString () s) = do
  l <- getStringLabel s
  return (Reg EAX, [
    IParam (Imm (fromIntegral $ length s)),
    IParam (Mem (MemoryGlobal l)),
    ICall (Ident "_allocString") 2])

genExpr (Neg () expr) = genUnOp NEG expr

genExpr expr@Not{} = genCondInit expr

genExpr (EAdd () expr1 (Plus ()) expr2) = genBinOp ADD expr1 expr2

genExpr (EAdd () expr1 (Minus ()) expr2) = genBinOp SUB expr1 expr2

genExpr (EMul () expr1 (Times ()) expr2) = genBinOp MUL expr1 expr2

genExpr (EMul () expr1 (Div ()) expr2) = genDivOp DIV expr1 expr2

genExpr (EMul () expr1 (Mod ()) expr2) = genDivOp MOD expr1 expr2

genExpr expr@ERel{} = genCondInit expr

genExpr expr@EAnd{} = genCondInit expr

genExpr expr@EOr{} = genCondInit expr

genBinOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBinOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> generr (oper, expr1, expr2)
    (Mem m1, Imm _) ->
      return (Reg EAX, [ILoad m1 EAX, IBinOp oper EAX o2])
    (Imm _, Mem m2) ->
      return (Reg EAX, if isCommutative oper
        then [ILoad m2 EAX, IBinOp oper EAX o1]
        else [IMov o1 EAX, IBinOp oper EAX o2])
    (Mem m1, Mem _) ->
      return (Reg EAX, [ILoad m1 EAX, IBinOp oper EAX o2])
    (Reg r1, Imm _) ->
      return (o1, i1 ++ [IBinOp oper r1 o2])
    (Imm _, Reg r2) -> return $
      if isCommutative oper
        then (o2, i2 ++ [IBinOp oper r2 o1])
        else (o2, i2 ++ [IMov o1 ECX, IBinOp oper ECX o2, IXchg ECX r2])
    (Reg r1, Mem _) ->
      return (Reg r1, i1 ++ [IBinOp oper r1 o2])
    (Mem m1, Reg r2) -> return $
      if isCommutative oper
        then (o2, i2 ++ [IBinOp oper r2 o1])
        else (o2, i2 ++ [ILoad m1 ECX, IBinOp oper ECX o2, IXchg ECX r2])
    (Reg r1, Reg r2) -> case compare r1 r2 of -- TODO całe + (EAX, EAX)
      GT -> return (o1, i1 ++ i2 ++ [IBinOp oper r1 o2])
      LT -> return (o2, i2 ++ i1 ++ [IXchg r1 r2, IBinOp oper r1 o2])
      EQ -> return $ case r1 of
        EAX -> (Reg EBX, i1 ++ [IMov o1 EBX] ++ i2 ++ [IBinOp oper EBX o2])
        ESI -> (Reg ESI, i2 ++ [IPush ESI] ++ i1 ++ [IPop EAX, IBinOp oper ESI (Reg EAX)])
        _ -> let r3 = nextReg r1 in
          (Reg r3, i1 ++ [IMov o1 r3] ++ i2 ++ [IBinOp oper r3 o2])

genDivOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genDivOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  let reg = max (toReg o1) (toReg o2)
      i3 = i1 ++ [IParam o1] ++ i2 ++ [IMov o2 ECX, IPop EAX, IBinOp oper EAX (Reg ECX)]
          ++ [IMov (Reg (if oper == DIV then EAX else EDX)) reg]
  return (Reg reg, i3)

genUnOp :: UnaryOperator -> Expr () -> ExpressionGenerator
genUnOp oper expr = do
  (o, i) <- genExpr expr
  case o of
    Imm _ -> generr (oper, expr)
    Mem m -> return (Reg EAX, i ++ [ILoad m EAX, IUnOp oper (Reg EAX)])
    Reg _ -> return (o, i ++ [IUnOp oper o])

-- Jumping code --

genCond :: Expr () -> Label -> Label -> Generator [Instruction]

genCond expr@EVar{} ltrue lfalse =
  genRelOp REQ expr (ELitTrue ()) ltrue lfalse

genCond ELitTrue{} ltrue _ = return [IJump ltrue]

genCond ELitFalse{} _ lfalse = return [IJump lfalse]

genCond expr@EApp{} ltrue lfalse =
  genRelOp REQ expr (ELitTrue ()) ltrue lfalse

genCond (Not () expr) ltrue lfalse = genCond expr lfalse ltrue

genCond (ERel () expr1 oper expr2) ltrue lfalse =
  genRelOp (mapRelOp oper) expr1 expr2 ltrue lfalse

genCond (EAnd () expr1 expr2) ltrue lfalse = do
  lmid <- lift $ lift newLabel
  i1 <- genCond expr1 lmid lfalse
  i2 <- genCond expr2 ltrue lfalse
  return (i1 ++ [ILabel lmid] ++ i2)

genCond (EOr () expr1 expr2) ltrue lfalse = do
  lmid <- lift $ lift newLabel
  i1 <- genCond expr1 ltrue lmid
  i2 <- genCond expr2 ltrue lfalse
  return (i1 ++ [ILabel lmid] ++ i2)

genCond expr ltrue lfalse = generr (expr, ltrue, lfalse)

genCondInit :: Expr () -> ExpressionGenerator
genCondInit expr = do
  ltrue <- lift $ lift newLabel
  lfalse <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  i <- genCond expr ltrue lfalse
  return (Reg EAX, i ++ [ILabel ltrue, IMov (Imm 1) EAX, IJump lafter,
    ILabel lfalse, IMov (Imm 0) EAX, ILabel lafter])

genRelOp :: RelationOperator -> Expr () -> Expr () ->
            Label -> Label -> Generator [Instruction]
genRelOp oper expr1 expr2 ltrue lfalse = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> generr (oper, expr1, expr2, ltrue, lfalse)
    (Imm _, Mem m2) ->
      return [ILoad m2 EAX, IJumpCond (revRelOp oper) EAX o1 ltrue, IJump lfalse]
    (Mem m1, Imm _) ->
      return [ILoad m1 EAX, IJumpCond oper EAX o2 ltrue, IJump lfalse]
    (Mem m1, Mem _) ->
      return [ILoad m1 EAX, IJumpCond oper EAX o2 ltrue, IJump lfalse]
    (Reg r1, Imm _) ->
      return (i1 ++ [IJumpCond oper r1 o2 ltrue, IJump lfalse])
    (Imm _, Reg r2) ->
      return (i2 ++ [IJumpCond (revRelOp oper) r2 o1 ltrue, IJump lfalse])
    (Reg r1, Mem _) ->
      return (i1 ++ [IJumpCond oper r1 o2 ltrue, IJump lfalse])
    (Mem _, Reg r2) ->
      return (i2 ++ [IJumpCond (revRelOp oper) r2 o1 ltrue, IJump lfalse])
    (Reg r1, Reg r2) -> case compare r1 r2 of -- TODO do something
      GT -> return (i1 ++ i2 ++ [IJumpCond oper r1 o2 ltrue, IJump lfalse])
      LT -> return (i2 ++ i1 ++ [IJumpCond (revRelOp oper) r2 o1 ltrue, IJump lfalse])
      EQ -> return $ case r1 of
        EAX -> i1 ++ [IMov o1 EBX] ++ i2 ++ [IJumpCond oper EBX o2 ltrue, IJump lfalse]
        ESI -> i2 ++ [IPush ESI] ++ i1 ++
          [IPop EAX, IJumpCond oper ESI (Reg EAX) ltrue, IJump lfalse]
        _ -> let r3 = nextReg r1 in i1 ++ [IMov o1 r3] ++ i2 ++
          [IJumpCond oper r3 o2 ltrue, IJump lfalse]
