module Backend.IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Prelude hiding (lookup)
import Data.Map (Map, empty, (!), lookup, insert, assocs)

import Parser.AbsLatte


type Size = Integer

type Pointer = Integer

data Label = Label Integer | StringLabel Integer

instance Show Label where
  show (Label n) = ".L" ++ show n
  show (StringLabel n) = ".LC" ++ show n

data Register = EBP | ESP | EAX | ECX | EDX | EBX | EDI | ESI
  deriving (Eq, Ord, Enum, Bounded)

instance Show Register where
  show EBP = "%ebp"
  show ESP = "%esp"
  show EAX = "%eax"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EBX = "%ebx"
  show EDI = "$edi"
  show ESI = "%esi"

data Memory
  = MemoryArgument Integer
  | MemoryLocal Integer
  | MemoryOffset Register Integer
  | MemoryGlobal Label

instance Show Memory where
  show (MemoryArgument n) = show n ++ "(" ++ show EBP ++ ")"
  show (MemoryLocal n) = "-" ++ show n ++ "(" ++ show EBP ++ ")"
  show (MemoryOffset r n) = "-" ++ show n ++ "(" ++ show EBP ++ ", " ++
                            show r ++ ", " ++ show wordLen ++ ")"
  show (MemoryGlobal l) = "$" ++ show l

data Operand
  = Reg Register
  | Mem Memory
  | Imm Integer

instance Show Operand where
  show (Reg r) = show r
  show (Mem m) = show m
  show (Imm n) = "$" ++ show n

data BinaryOperator = ADD | SUB | MUL | DIV | MOD deriving Eq

instance Show BinaryOperator where
  show ADD = "addl"
  show SUB = "subl"
  show MUL = "imull"
  show DIV = "idivl"
  show MOD = "idivl"

data RelationOperator = REQ | RNE | RGT | RGE | RLT | RLE deriving Eq

instance Show RelationOperator where
  show REQ = "je"
  show RNE = "jne"
  show RGT = "jg"
  show RGE = "jge"
  show RLT = "jl"
  show RLE = "jle"

data UnaryOperator = NEG | INC | DEC deriving Eq

instance Show UnaryOperator where
  show NEG = "negl"
  show INC = "incl"
  show DEC = "decl"

data Instruction
  = IMov Operand Register
  | ILoad Memory Register
  | IStore Operand Memory
  | IXchg Register Register
  | IPush Register
  | IPop Register
  | IParam Operand
  | ICall Ident Integer
  | IBinOp BinaryOperator Register Operand
  | IUnOp UnaryOperator Operand
  | IJump Label
  | IJumpCond RelationOperator Register Operand Label
  | IRet
  | ILabel Label

instance Show Instruction where
  show (IMov o1 r2) = showDouble "movl" o1 r2
  show (ILoad m1 r2) = showDouble "movl" m1 r2
  show (IStore o1 m2) = showDouble "movl" o1 m2
  show (IXchg r1 r2) = showDouble "xchgl" r1 r2
  show (IPush r) = showSingle "push" r
  show (IPop r) = showSingle "pop" r
  show (IParam o) = showSingle "push" o
  show (ICall (Ident s) n) =
    "\tcall " ++ s ++ nextins (IBinOp ADD ESP (Imm (n*wordLen)))
  show (IBinOp DIV _ o) = "\tcdq\n" ++ showSingle (show DIV) o
  show (IBinOp MOD _ o) = "\tcdq\n" ++ showSingle (show MOD) o
  -- show (IBinOp DIV r1 o2) = -- TODO poprawić dzielenie
  --   show (IPush EDX) ++ nextins (IPush EAX) ++ nextins (IMov (Reg r1) EAX)
  --   ++ "\n\tcdq\n" ++ showSingle (show DIV) o2 ++ nextins (IMov (Reg EAX) r1)
  --   ++ nextins (IPop EAX) ++ nextins (IPop EDX)
  -- show (IBinOp MOD r1 o2) =
  --   show (IPush EDX) ++ nextins (IPush EAX) ++ nextins (IMov (Reg r1) EAX)
  --   ++ "\n\tcdq\n" ++ showSingle (show MOD) o2 ++ nextins (IMov (Reg EDX) r1)
  --   ++ nextins (IPop EAX) ++ nextins (IPop EDX)
  show (IBinOp op r1 o2) = showDouble (show op) o2 r1
  show (IUnOp op o) = showSingle (show op) o
  show (IJump l) = showSingle "jmp" l
  show (IJumpCond op r1 o2 l) =
    showDouble "cmpl" o2 r1 ++ "\n" ++ showSingle (show op) l
  show IRet = "\tret"
  show (ILabel l) = show l ++ ":"

nextins :: Show a => a -> String
nextins ins = "\n" ++ show ins

showSingle :: Show a => String -> a -> String
showSingle ins arg = "\t" ++ ins ++ " " ++ show arg

showDouble :: (Show a, Show b) => String -> a -> b -> String
showDouble ins arg1 arg2 = "\t" ++ ins ++ " " ++ show arg1 ++ ", " ++ show arg2

data Environment = Environment {
  labels :: Maybe (Label, Label) -- TODO unused
}

data Store = Store {
  labelCounter :: Integer,
  localSize :: Size,
  variableEnv :: Map Ident Memory,
  stringCounter :: Integer,
  stringEnv :: Map String Label,
  returnLabel :: Label
}

data ImmediateCode = ImmediateCode {
  functions :: [(Ident, [Instruction])],
  strings :: [(String, Label)]
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
  stringEnv = empty,
  returnLabel = Label 0
}

wordLen :: Size
wordLen = 4

firstReg :: Register
firstReg = EAX

firstSaveReg :: Register
firstSaveReg = EBX

lastReg :: Register
lastReg = maxBound

nextReg :: Register -> Register
nextReg = succ

toReg :: Operand -> Register
toReg (Reg r) = r
toReg _ = EAX

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
getArgEnv args = fst $ foldl addArg (empty, 2 * wordLen) args where
  addArg (env, size) (Arg () _ argid) =
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
epilog lab reg = [ILabel lab] ++ pops ++ [IMov (Reg EBP) ESP, IPop EBP, IRet] where
  pops = case reg of
    EBX -> [IPop EBX]
    EDI -> [IPop EDI, IPop EBX]
    ESI -> [IPop ESI, IPop EDI, IPop EBX]
    _ -> []

newLabel :: TopGenerator Label
newLabel = state (\s -> let n = labelCounter s in (Label n, s {labelCounter = n + 1}))

generr :: Show a => a -> Generator b
generr err = error $ "Unexpected error\n" ++ show err

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


runIntermediateCodeGeneration :: Program () -> IO ImmediateCode
runIntermediateCodeGeneration program = evalStateT (genProgram program) emptyStore

genProgram :: Program () -> TopGenerator ImmediateCode
genProgram (Program () topdefs) = do
  fs <- mapM genTopDef topdefs
  store <- get
  let strs = assocs (stringEnv store)
  return (ImmediateCode fs strs)

genTopDef :: TopDef () -> TopGenerator (Ident, [Instruction])
genTopDef (FnDef () _ ident args block) = do
  let env = getArgEnv args
  l <- newLabel
  s1 <- get
  put (s1 {localSize = 0, variableEnv = env, returnLabel = l})
  ins <- execWriterT (runReaderT (genBlock block) emptyEnvironment)
  s2 <- get
  let callieSave = usedCallieSave ins
      fullIns = prolog (localSize s2) callieSave ++ ins ++ epilog l callieSave
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

genExpr :: Expr () -> ExpressionGenerator

genExpr (EVar () ident) = getLoc ident >>= \m -> return (Mem m, [])

genExpr (ELitInt () n) = return (Imm n, [])

genExpr (ELitTrue ()) = return (Imm 1, [])

genExpr (ELitFalse ()) = return (Imm 0, [])

genExpr (EApp () ident exprs) = do
  params <- mapM genExpr exprs
  -- let ins = concatMap (\(o, i) -> i ++ [IParam o]) params
  let ins = foldl (\ins' (o, i) -> i ++ [IParam o] ++ ins') [] params
  let reg = if null params then EAX else maximum $ map (toReg . fst) params
  return (Reg reg, ins ++ [ICall ident (fromIntegral $ length params), IMov (Reg EAX) reg])

  -- return (Reg EAX, ins ++ [ICall ident (fromIntegral $ length params)])
  -- return (Reg EAX, [IPush EDX, IPush ECX] ++ ins ++
  --   [ICall ident (fromIntegral $ length params)] ++ [IPop ECX, IPop EDX])


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
  -- let i3 = [IPush EDX, IPush ECX] ++ i1 ++ [IParam o1] ++ i2 ++
  --       [IMov o2 ECX, IPop EAX, IBinOp oper EAX (Reg ECX)] ++
  --       [IMov (Reg EDX) EAX | oper == MOD] ++ [IPop ECX, IPop EDX]
  return (Reg reg, i3)
  -- push edx ecx
  -- IBinOp EAX (Reg ECX)
  -- pop
  -- case (o1, o2) of
  --   (Reg r1, Reg r2) -> case compare r1 r2 of
  --     GT -> i1 ++ i2 ++ [IMov o1 EAX, IMov o2 ECX]
  --     LT -> i2 ++ i1 ++ [IMov o1 EAX, IMov o2 ECX]

-- genDivOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
-- genDivOp oper expr1 expr2 = do
--   (o1, i1) <- genExpr expr1
--   (o2, i2) <- genExpr expr2
--     case (o1, o2) of
--       (Imm _, Imm _) -> generr (oper, expr1, expr2)
--       (Mem m1, Imm _) ->
--         return (Reg EAX, [IPush EDX, IPush ECX, ILoad m1 EAX, IMov o2 ECX, IBinOp oper EAX (Reg ECX), IPop ECX, IPop EDX])
--       (Imm _, Mem m2) ->
--         return (Reg EAX, [IPush EDX, IMov o1 EAX, IBinOp oper EAX o2, IPop EDX])
--       (Mem m1, Mem _) ->
--         return (Reg EAX, [IPush EDX, ILoad m1 EAX, IBinOp oper EAX o2, IPop EDX])
--       (Reg r1, Imm _) ->
--         return (Reg r1, [IPush EDX, IPush ECX, IMov o1 EAX, IMov o2 ECX, IBinOp oper EAX (Reg ECX), IPop ECX, IPop EDX, IMov EAX o1])
--       (Imm _, Reg r2) ->
--         return (Reg r2, [IPush EDX, IPush ECX, IMov o1 EAX, IBinOp oper EAX o2])

genUnOp :: UnaryOperator -> Expr () -> ExpressionGenerator
genUnOp oper expr = do
  (o, i) <- genExpr expr
  case o of
    Imm _ -> generr (oper, expr)
    Mem m -> return (Reg firstReg, i ++ [ILoad m firstReg, IUnOp oper (Reg firstReg)])
    Reg _ -> return (o, i ++ [IUnOp oper o])

genRelOp :: RelationOperator -> Expr () -> Expr () ->
            Label -> Label -> Generator [Instruction]
genRelOp oper expr1 expr2 ltrue lfalse = do
  (o1, i1) <- genExpr expr1
  (o2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> generr (oper, expr1, expr2, ltrue, lfalse)
    (Imm _, Mem m2) ->
      return [ILoad m2 firstReg, IJumpCond (revRelOp oper) firstReg o1 ltrue, IJump lfalse]
    (Mem m1, Imm _) ->
      return [ILoad m1 firstReg, IJumpCond oper firstReg o2 ltrue, IJump lfalse]
    (Mem m1, Mem _) ->
      return [ILoad m1 firstReg, IJumpCond oper firstReg o2 ltrue, IJump lfalse]
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

      -- return $ if r1 == lastReg
      --   then let r3 = firstReg in
      --     (i2 ++ [IPush r2] ++ i1 ++
      --     [IPop r3, IJumpCond oper r1 (Reg r3) ltrue, IJump lfalse])
      --   else let r3 = nextReg r1 in
      --     (i1 ++ [IMov o1 r3] ++ i2 ++ [IJumpCond oper r3 o2 ltrue, IJump lfalse])
      --
      --
      --     EAX -> (Reg EBX, i1 ++ [IMov o1 EBX] ++ i2 ++ [IBinOp oper EBX o2])
      --     ESI -> (Reg ESI, i2 ++ [IPush ESI] ++ i1 ++ [IPop EAX, IBinOp oper ESI (Reg EAX)])
      --     _ -> let r3 = nextReg r1 in
      --       (Reg r3, i1 ++ [IMov o1 r3] ++ i2 ++ [IBinOp oper r3 o2])


genCondInit :: Expr () -> ExpressionGenerator
genCondInit expr = do
  ltrue <- lift $ lift newLabel
  lfalse <- lift $ lift newLabel
  lafter <- lift $ lift newLabel
  i <- genCond expr ltrue lfalse
  return (Reg firstReg, i ++ [ILabel ltrue, IMov (Imm 1) firstReg, IJump lafter,
    ILabel lfalse, IMov (Imm 0) firstReg, ILabel lafter])

genCond :: Expr () -> Label -> Label -> Generator [Instruction]

genCond expr@EVar{} ltrue lfalse =
  genRelOp REQ expr (ELitTrue ()) ltrue lfalse

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

mapRelOp :: RelOp () -> RelationOperator
mapRelOp (LTH ()) = RLT
mapRelOp (LE ()) = RLE
mapRelOp (GTH ()) = RGT
mapRelOp (GE ()) = RGE
mapRelOp (EQU ()) = REQ
mapRelOp (NE ()) = RNE

-- negRelOp :: RelationOperator -> RelationOperator
-- negRelOp REQ = RNE
-- negRelOp RNE = REQ
-- negRelOp RGT = RLE
-- negRelOp RGE = RLT
-- negRelOp RLT = RGE
-- negRelOp RLE = RGT

revRelOp :: RelationOperator -> RelationOperator
revRelOp RGT = RLT
revRelOp RGE = RLE
revRelOp RLT = RGT
revRelOp RLE = RGE
revRelOp oper = oper
