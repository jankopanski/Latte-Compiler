module IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Map (Map, empty, (!), insert)
-- import Data.Maybe (isJust, fromJust)

import AbsLatte


type Size = Integer

type Pointer = Integer

newtype Label = Label Integer

data Register = EBP | ESP | EAX | ECX | EDX | EBX | EDI | ESI
  deriving (Eq, Ord, Enum, Bounded)

data Memory
  = MemoryArgument Integer
  | MemoryLocal Integer
  | MemoryOffset Register Integer
  | MemoryGlobal

data Operand
  = Reg Register
  | Mem Memory
  | Imm Integer
  | Nop

data BinaryOperator = ADD | SUB | MUL | DIV | MOD | AND | OR deriving (Eq)
data RelationOperator = REQ | RNE | RGT | RGE | RLT | RLE deriving (Eq)
data UnaryOperator = NEG | NOT | INC | DEC deriving (Eq)

data Instruction
  = IMov Operand Register
  | ILoad Memory Register
  | IStore Operand Memory
  | IPush Register
  | IPop Register
  | IXchg Register Register
  | IBinOp BinaryOperator Register Operand
  -- | IRelOp RelationOperator Operand Operand
  | IUnOp UnaryOperator Operand
  | IParam Operand
  | ICall Ident Integer
  | IJump Label
  | IJumpCond RelationOperator Register Operand Label
  | IRet
  | ILabel Label

data Environment = Environment {
  labels :: Maybe (Label, Label)
}

data Store = Store {
  labelCounter :: Integer,
  localSize :: Size,
  variableEnv :: Map Ident Memory,
  stringCounter :: Integer,
  strings :: [String],
  returnLabel :: Label
}

data ImmediateCode = ImmediateCode {
  functions :: [(Ident, [Instruction])]
}

type TopGeneratorT = StateT Store IO
type TopGenerator a = TopGeneratorT a
type Generator a =  ReaderT Environment (WriterT [Instruction] TopGeneratorT) a
type StatementGenerator = Generator ()
type ExpressionGenerator = Generator (Operand, [Instruction])

emptyEnvironment :: Environment
emptyEnvironment = Environment {
  labels = Nothing
}

emptyStore :: Store
emptyStore = Store {
  labelCounter = 0,
  localSize = 0,
  variableEnv = empty,
  stringCounter = 0,
  strings = [],
  returnLabel = Label 0
}

wordLen :: Size
wordLen = 4

firstReg :: Register
firstReg = EAX

lastReg :: Register
lastReg = maxBound

nextReg :: Register -> Register
nextReg = succ

-- prevReg :: Register -> Register
-- prevReg = pred

isCommutative :: BinaryOperator -> Bool
isCommutative ADD = True
isCommutative MUL = True
isCommutative _ = False

sizeOf :: Type () -> Integer
sizeOf _ = wordLen

defaultValue :: Type () -> Integer
defaultValue _ = 0

getArgEnv :: [Arg ()] -> Map Ident Memory
getArgEnv args = fst $ foldr addArg (empty, 2 * wordLen) args where
  addArg (Arg () _ argid) (env, size) =
    (insert argid (MemoryArgument size) env, size + wordLen)

usedCallieSave :: [Instruction] -> Register
usedCallieSave = foldl scanIns firstReg where
  scanIns mr (IMov _ r) = max mr r
  scanIns mr _ = mr

prolog :: Size -> Register -> [Instruction]
prolog size reg = [IPush EBP, IMov (Reg ESP) EBP, IBinOp SUB ESP (Imm size)] ++
  case reg of
    EBX -> [IPush EBX]
    EDI -> [IPush EBX, IPush EDI]
    ESI -> [IPush EBX, IPush EDI, IPush ESI]
    _ -> []

epilog :: Label -> Register -> [Instruction]
epilog lab reg = pops ++ [ILabel lab, IMov (Reg EBP) ESP, IPop EBP, IRet] where
  pops = case reg of
    EBX -> [IPop EBX]
    EDI -> [IPop EDI, IPop EBX]
    ESI -> [IPop ESI, IPop EDI, IPop EBX]
    _ -> []

newLabel :: TopGenerator Label
newLabel = state (\s -> let n = labelCounter s in (Label n, s {labelCounter = n + 1}))

generr :: Generator a
generr = error "Unexpected error"

newLoc :: Type () -> Generator Memory
newLoc vartype = state (\s -> let size = localSize s in
  (MemoryLocal (-(size + wordLen)), s {localSize = size + sizeOf vartype}))

getLoc :: Ident -> Generator Memory
getLoc ident = get >>= \s -> return $ variableEnv s ! ident


runIntermediateCodeGeneration :: Program () -> IO ImmediateCode
runIntermediateCodeGeneration program = evalStateT (genProgram program) emptyStore

genProgram :: Program () -> TopGenerator ImmediateCode
genProgram (Program () topdefs) = do
  fs <- mapM genTopDef topdefs
  return (ImmediateCode fs)

genTopDef :: TopDef () -> TopGenerator (Ident, [Instruction])
genTopDef (FnDef () _ ident args block) = do
  let env = getArgEnv args
  l <- newLabel
  s <- get
  put (s {localSize = 0, variableEnv = env, returnLabel = l})
  ins <- execWriterT (runReaderT (genBlock block) emptyEnvironment)
  let callieSave = usedCallieSave ins
      fullIns = prolog (localSize s) callieSave ++ ins ++ epilog l callieSave
  return (ident, fullIns)

genBlock :: Block () -> StatementGenerator
genBlock (Block () stmts) = do
  s1 <- get
  mapM_ genStmt stmts
  s2 <- get
  put (s2 {variableEnv = variableEnv s1}) -- TODO dodać przywracanie localSize, maxLoaclSize

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
    Mem m1 -> tell [ILoad m1 firstReg, IStore (Reg firstReg) m2]
    _ -> tell $ i ++ [IStore o m2]

genStmt (Incr () ident) = getLoc ident >>= \m -> tell [IUnOp INC (Mem m)]

genStmt (Decr () ident) = getLoc ident >>= \m -> tell [IUnOp DEC (Mem m)]

genStmt (Ret () expr) = do
  (o, i) <- genExpr expr
  s <- get
  tell $ i ++ [IMov o EAX, IJump (returnLabel s)]

genStmt (VRet ()) = get >>= \s -> tell [IJump (returnLabel s)]

genStmt (Cond () expr stmt) = do
  lthen <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  (_, i) <- local (\env -> env {labels = Just (lafter, lthen)}) (genExpr expr)
  tell $ i ++ [ILabel lthen]
  genStmt stmt
  tell [ILabel lafter]

genStmt (CondElse () expr stmt1 stmt2) = do
  lthen <- lift $ lift newLabel
  lelse <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  (_, i) <- local (\env -> env {labels = Just (lelse, lthen)}) (genExpr expr)
  tell $ i ++ [ILabel lthen]
  genStmt stmt1
  tell [ILabel lelse]
  genStmt stmt2
  tell [ILabel lafter]

genStmt (While () expr stmt) = do
  lcond <- lift $ lift newLabel
  lwhile <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  (_, i) <- local (\env -> env {labels = Just (lafter, lwhile)}) (genExpr expr)
  tell $ [ILabel lcond] ++ i ++ [ILabel lwhile]
  genStmt stmt
  tell [ILabel lafter]

genStmt (SExp () expr) = genExpr expr >>= \(_, i) -> tell i

genExpr :: Expr () -> ExpressionGenerator

genExpr (EVar () ident) = getLoc ident >>= \m -> return (Mem m, [])

genExpr (ELitInt () n) = return (Imm n, [])

genExpr (ELitTrue ()) = return (Imm 1, [])

genExpr (ELitFalse ()) = return (Imm 0, [])

genExpr (EApp () ident exprs) = do
  params <- mapM genExpr exprs
  let ins = concatMap (\(o, i) -> i ++ [IParam o]) params
  return (Reg EAX, ins ++ [ICall ident (fromIntegral $ length params)])
-- genExpr (EString () s) = do
--   l <- addGlobalString s
--

genExpr (Neg () expr) = genUnOp NEG expr

genExpr (Not () expr) = genUnOp NOT expr

genExpr (EAdd () expr1 (Plus ()) expr2) = genBinOp ADD expr1 expr2

genExpr (EAdd () expr1 (Minus ()) expr2) = genBinOp SUB expr1 expr2

genExpr (EMul () expr1 (Times ()) expr2) = genBinOp MUL expr1 expr2

genExpr (EMul () expr1 (Div ()) expr2) = genBinOp DIV expr1 expr2

genExpr (EMul () expr1 (Mod ()) expr2) = genBinOp MOD expr1 expr2

genExpr (ERel () expr1 (LTH ()) expr2) = genRelOp RLT expr1 expr2

genExpr (ERel () expr1 (LE ()) expr2) = genRelOp RLE expr1 expr2

genExpr (ERel () expr1 (GTH ()) expr2) = genRelOp RGT expr1 expr2

genExpr (ERel () expr1 (GE ()) expr2) = genRelOp RGE expr1 expr2

genExpr (ERel () expr1 (EQU ()) expr2) = genRelOp REQ expr1 expr2

genExpr (ERel () expr1 (NE ()) expr2) = genRelOp RNE expr1 expr2

genExpr (EAnd () expr1 expr2) = genBoolOp AND expr1 expr2

genExpr (EOr () expr1 expr2) = genBoolOp OR expr1 expr2

genBinOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBinOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Mem m1, Imm _) ->
      return (Reg firstReg, [ILoad m1 firstReg, IBinOp oper firstReg o2])
    (Imm _, Mem m2) ->
      return (Reg firstReg, if isCommutative oper
        then [ILoad m2 firstReg, IBinOp oper firstReg o1]
        else [IMov o1 firstReg, IBinOp oper firstReg o2])
    (Mem m1, Mem _) ->
      return (Reg firstReg, [ILoad m1 firstReg, IBinOp oper firstReg o2])
    (Reg r1, Imm _) ->
      return (o1, i1 ++ [IBinOp oper r1 o2])
    (Imm _, Reg r2) -> return $
      if isCommutative oper
        then (o2, i2 ++ [IBinOp oper r2 o1])
      else if r2 == firstReg
        then let r3 = nextReg r2 in (Reg r3, i2 ++ [IMov o1 r3, IBinOp oper r3 o2])
      else (o2, i2 ++ [IMov o1 firstReg, IBinOp oper firstReg o2, IXchg firstReg r2])
    (Reg r1, Mem _) ->
      return (Reg r1, i1 ++ [IBinOp oper r1 o2])
    (Mem m1, Reg r2) -> return $
      if isCommutative oper
        then (o2, i2 ++ [IBinOp oper r2 o1])
      else if r2 == firstReg
        then let r3 = nextReg r2 in (Reg r3, i2 ++ [ILoad m1 r3, IBinOp oper r3 o2])
      else (o2, i2 ++ [ILoad m1 firstReg, IBinOp oper firstReg o2, IXchg firstReg r2])
    (Reg r1, Reg r2) -> case compare r1 r2 of
      GT -> return (Reg r1, i1 ++ i2 ++ [IBinOp oper r1 o2])
      LT -> return (Reg r2, i2 ++ i1 ++ [IXchg r1 r2, IBinOp oper r1 o2])
      EQ -> return $ if r1 == lastReg
        then let r3 = firstReg in
          (o1, i2 ++ [IPush r2] ++ i1 ++ [IPop r3, IBinOp oper r1 (Reg r3)])
        else let r3 = nextReg r1 in
          (Reg r3, i1 ++ [IMov o1 r3] ++ i2 ++ [IBinOp oper r3 o2])
    _ -> generr

genUnOp :: UnaryOperator -> Expr () -> ExpressionGenerator
genUnOp oper expr = do
  (o, i) <- genExpr expr
  case o of
    Mem m -> return (Reg firstReg, i ++ [ILoad m firstReg, IUnOp oper (Reg firstReg)])
    Reg _ -> return (o, i ++ [IUnOp oper o])
    _ -> generr

genRelOp :: RelationOperator -> Expr () -> Expr () -> ExpressionGenerator
genRelOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  (l1, l2) <- getLabels
  case (o1, o2) of
    (Imm _, Mem m2) ->
      return (Nop, [ILoad m2 firstReg, IJumpCond (neg oper) firstReg o1 l2, IJump l1])
    (Mem m1, Imm _) ->
      return (Nop, [ILoad m1 firstReg, IJumpCond oper firstReg o2 l1, IJump l2])
    (Mem m1, Mem _) ->
      return (Nop, [ILoad m1 firstReg, IJumpCond oper firstReg o2 l1, IJump l2])
    (Reg r1, Imm _) ->
      return (Nop, i1 ++ [IJumpCond oper r1 o2 l1, IJump l2])
    (Imm _, Reg r2) ->
      return (Nop, i2 ++ [IJumpCond (neg oper) r2 o1 l2, IJump l1])
    (Reg r1, Mem _) ->
      return (Nop, i1 ++ [IJumpCond oper r1 o2 l1, IJump l2])
    (Mem _, Reg r2) ->
      return (Nop, i2 ++ [IJumpCond (neg oper) r2 o1 l2, IJump l1])
    (Reg r1, Reg _) ->
      return (Nop, i1 ++ [IJumpCond oper r1 o2 l1, IJump l2])
    _ -> generr

genBoolOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBoolOp oper expr1 expr2 = do
  (l1, l2) <- getLabels
  l3 <- lift $ lift newLabel
  let ll = if oper == AND then (l1, l3) else (l3, l2)
  (o1, i1) <- local (\env -> env {labels = Just ll}) (genExpr expr1)
  (o2, i2) <- local (\env -> env {labels = Just (l1, l2)}) (genExpr expr2)
  let i1' = case o1 of
        Nop -> i1
        _ -> i1 ++ [IMov o1 firstReg, IJumpCond REQ firstReg (Imm 0) (fst ll), IJump (snd ll)]
      i2' = case o2 of
        Nop -> i2
        _ -> i2 ++ [IMov o2 firstReg, IJumpCond REQ firstReg (Imm 0) l1, IJump l2]
  return (Nop, i1' ++ i2')

getLabels :: Generator (Label, Label)
getLabels = do
  env <- ask
  case labels env of
    Just ll -> return ll
    Nothing -> do
      l1 <- lift $ lift newLabel
      l2 <- lift $ lift newLabel
      return (l1, l2)

neg :: RelationOperator -> RelationOperator
neg REQ = RNE
neg RNE = REQ
neg RGT = RLE
neg RGE = RLT
neg RLT = RGE
neg RLE = RGT
