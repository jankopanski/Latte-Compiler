module IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map, empty, (!), insert)

import AbsLatte

newtype Label = Label Integer

type Size  = Integer

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

data BinaryOperator = ADD | SUB | MUL | DIV | AND | OR deriving (Eq)
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
  | ICall Label Integer
  | IJump Label
  | IJumpCond Operand Label
  | IRet
  | ILable Label

data Store = Store {
  labelCounter :: Integer,
  localSize :: Size,
  variableEnv :: Map Ident Memory,
  returnLabel :: Label
}

type RegisterUsage = [(Register, Bool)]

type TopGeneratorT = StateT Store IO
type TopGenerator a = TopGeneratorT a
type Generator a = WriterT [Instruction] TopGeneratorT a
type StatementGenerator = Generator ()
type ExpressionGenerator = Generator (Operand, [Instruction])

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
sizeOf (Int ()) = wordLen

defaultValue :: Type () -> Integer
defaultValue (Int ()) = 0

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

allocate :: Type () -> Generator Memory
allocate vartype = state (\s -> let size = localSize s in
  (MemoryLocal (-(size + wordLen)), s {localSize = size + sizeOf vartype}))

getLoc :: Ident -> Generator Memory
getLoc ident = get >>= \s -> return $ variableEnv s ! ident

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
    m <- allocate vartype
    s <- get
    put (s {variableEnv = insert ident m (variableEnv s)})
    tell [IStore (Imm (defaultValue vartype)) m]
  genDecl (Init () ident expr) = do
    (o, i) <- genExpr expr
    m <- allocate vartype
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

genExpr (ELitInt () n) = return (Imm n, [])

genExpr (ELitTrue ()) = return (Imm 1, [])

genExpr (ELitFalse ()) = return (Imm 0, [])

genExpr (EAdd () expr1 (Plus ()) expr2) = genBinOp ADD expr1 expr2

genBinOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBinOp oper expr1 expr2 = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> generr
    (Mem m1, Imm c2) ->
      return (Reg firstReg, [ILoad m1 firstReg, IBinOp oper firstReg (Imm c2)])
    (Imm c1, Mem m2) ->
      return (Reg firstReg, if isCommutative oper
        then [ILoad m2 firstReg, IBinOp oper firstReg (Imm c1)]
        else [IMov (Imm c1) firstReg, IBinOp oper firstReg (Mem m2)])
    (Mem m1, Mem m2) ->
      return (Reg firstReg, [ILoad m1 firstReg, IBinOp oper firstReg (Mem m2)])
    (Reg r1, Imm c2) ->
      return (Reg r1, i1 ++ [IBinOp oper r1 (Imm c2)])
    (Imm c1, Reg r2) -> return $
      if isCommutative oper
        then (Reg r2, i2 ++ [IBinOp oper r2 (Imm c1)])
      else if r2 == firstReg
        then let r3 = nextReg r2 in
          (Reg r3, i2 ++ [IMov (Imm c1) r3, IBinOp oper r3 (Reg r2)])
      else (Reg r2, i2 ++ [IMov (Imm c1) firstReg,
          IBinOp oper firstReg (Reg r2), IXchg firstReg r2])
    (Reg r1, Mem m2) -> return (Reg r1, i1 ++ [IBinOp oper r1 (Mem m2)])
    (Mem m1, Reg r2) -> return $
      if isCommutative oper
        then (Reg r2, i2 ++ [IBinOp oper r2 (Mem m1)])
      else if r2 == firstReg
        then let r3 = nextReg r2 in (Reg r3, i2 ++ [ILoad m1 r3, IBinOp oper r3 (Reg r2)])
      else (Reg r2, i2 ++ [ILoad m1 firstReg, IBinOp oper firstReg (Reg r2), IXchg firstReg r2])
    (Reg r1, Reg r2) -> case compare r1 r2 of
      GT -> return (Reg r1, i1 ++ i2 ++ [IBinOp oper r1 (Reg r2)])
      LT -> return (Reg r2, i2 ++ i1 ++ [IXchg r1 r2, IBinOp oper r1 (Reg r2)])
      EQ -> return $ if r1 == lastReg
        then let r3 = firstReg in
          (Reg r1, i2 ++ [IPush r2] ++ i1 ++ [IPop r3, IBinOp oper r1 (Reg r3)])
        else let r3 = nextReg r1 in
          (Reg r3, i1 ++ [IMov (Reg r1) r3] ++ i2 ++ [IBinOp oper r3 (Reg r2)])
