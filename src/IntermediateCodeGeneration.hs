module IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map, empty, (!), insert)

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

data BinaryOperator = ADD | SUB | MUL | DIV | MOD | AND | OR deriving (Eq)
-- data RelationOperator = EQ | NE | GT | GE | LT | LE deriving (Eq)
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
  | IJumpCond Operand Label
  | IRet
  | ILable Label

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
type Generator a = WriterT [Instruction] TopGeneratorT a
type StatementGenerator = Generator ()
type ExpressionGenerator = Generator (Operand, [Instruction])

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
epilog lab reg = pops ++ [ILable lab, IMov (Reg EBP) ESP, IPop EBP, IRet] where
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
  ins <- execWriterT (genBlock block)
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

genExpr (EAnd () expr1 expr2) = genBinOp AND expr1 expr2

genExpr (EOr () expr1 expr2) = genBinOp OR expr1 expr2

genBinOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBinOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> generr
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

genUnOp :: UnaryOperator -> Expr () -> ExpressionGenerator
genUnOp oper expr = do
  (o, i) <- genExpr expr
  case o of
    Imm _ -> generr
    Mem m -> return (Reg firstReg, i ++ [ILoad m firstReg, IUnOp oper (Reg firstReg)])
    Reg _ -> return (o, i ++ [IUnOp oper o])
