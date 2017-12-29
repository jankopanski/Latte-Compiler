module IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as Map

import AbsLatte


-- class Operand a
--
-- instance Operand Register
--
-- data (Operand a) => Memory a = MemoryArgument Int | MemoryLocal Int
-- data Location
--   = Register ArchRegister
--   | Immediate Int
--   | MemoryArgument Int
--   | MemoryLocal Int
--   | MemoryOffset Location Int
-- | BinOp { binop :: BinaryOperator, src :: Operand, dst :: Operand }
-- data Immediate = Immediate Int
-- data Operand = Immediate | Register | Memory
newtype Label = Label Int

data Register = EAX | EBX | ECX | EDX | EDI | ESI deriving (Eq, Ord)

data Memory
  = MemoryArgument Int
  | MemoryLocal Int
  | MemoryOffset Register Int
  | MemoryGlobal

data Operand
  = Reg Register
  | Mem Memory
  | Imm Integer

data BinaryOperator = ADD | SUB | MUL | DIV | AND | OR
data RelationOperator = EQ | NE | GT | GE | LT | LE
data UnaryOperator = NEG | NOT | ID

data Instruction
  = ILoad Operand Register
  | IStore Operand Memory
  | IBinOp BinaryOperator Operand Operand
  | IRelOp RelationOperator Operand Operand
  | IUnOp UnaryOperator Operand
  | IParam Operand
  | ICall Label Int
  | IJump Label
  | IJumpCond Operand Label
  | ILable Label

data Store = Store {
  labelCounter :: Int,
  registerCounter :: Int,
  localSize :: Int,
  -- usedRegisters :: (Bool, Bool, Bool), -- (EBX, EDI, ESI)
  lru :: [(Integer, Register)],
  registerMap :: Map.Map Register Memory,
  variableEnv :: Map.Map Ident (Memory, Maybe Register)
}

-- type FunctionBlock = (Ident, [Instruction])
type TopGeneratorT = StateT Store IO
type TopGenerator a = TopGeneratorT a
type Generator a = WriterT [Instruction] TopGeneratorT a

-- freeRegister :: Generator Register
-- freeRegister = do
--   s <- get
--   let (ord, reg) = minimum (lru s)
--   let mem = (Map.! (registerMap s) reg)
--   store reg mem
--   return reg

store :: Register -> Memory -> Generator Register
store op mem = return EAX

load :: Operand -> Generator Register
load (Imm n) = return EAX

-- genExpr :: Expr () -> Generator Register
-- genExpr (ELitInt () n) = load (Imm n)
