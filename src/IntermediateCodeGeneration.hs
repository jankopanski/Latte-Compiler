module IntermediateCodeGeneration where

import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as Map

import AbsLatte

newtype Label = Label Int

data Register = EAX | ECX | EDX | EBX | EDI | ESI deriving (Eq, Ord, Enum, Bounded)

data Memory
  = MemoryArgument Int
  | MemoryLocal Int
  | MemoryOffset Register Int
  | MemoryGlobal

data Operand
  = Reg Register
  | Mem Memory
  | Imm Integer

data BinaryOperator = ADD | SUB | MUL | DIV | AND | OR deriving (Eq)
-- data RelationOperator = EQ | NE | GT | GE | LT | LE deriving (Eq)
data UnaryOperator = NEG | NOT | ID deriving (Eq)

data Instruction
  = IMov Operand Operand
  | ILoad Memory Register
  | IStore Operand Memory
  | IPush Register
  | IPop Register
  | IXchg Register Register
  | IBinOp BinaryOperator Operand Operand
  -- | IRelOp RelationOperator Operand Operand
  | IUnOp UnaryOperator Operand
  | IParam Operand
  | ICall Label Int
  | IJump Label
  | IJumpCond Operand Label
  | ILable Label

data Store = Store {
  labelCounter :: Int,
  localSize :: Int,
  variableEnv :: Map.Map Ident Memory
}

type RegisterUsage = [(Register, Bool)]

type TopGeneratorT = StateT Store IO
type TopGenerator a = TopGeneratorT a
type Generator a = WriterT [Instruction] TopGeneratorT a
type ExpressionGenerator = Generator (Operand, RegisterUsage, [Instruction])

allEmpty :: RegisterUsage
allEmpty = [
  (EAX, False),
  (ECX, False),
  (EDX, False),
  (EBX, False),
  (EDI, False),
  (ESI, False)]

setUsed :: Register -> RegisterUsage -> RegisterUsage
setUsed r = map (\(r', b) -> (r', r == r' || b))

singleUsed :: Register -> RegisterUsage
singleUsed r = setUsed r allEmpty

store :: Register -> Memory -> Generator Register
store op mem = return EAX

load :: Operand -> Generator Register
load (Imm n) = return EAX

genExpr :: Expr () -> ExpressionGenerator

genExpr (ELitInt () n) = return (Imm n, allEmpty, [])

genExpr (ELitTrue ()) = return (Imm 1, allEmpty, [])

genExpr (ELitFalse ()) = return (Imm 0, allEmpty, [])

genExpr (EAdd () expr1 (Plus ()) expr2) = genBinOp ADD expr1 expr2

genBinOp :: BinaryOperator -> Expr () -> Expr () -> ExpressionGenerator
genBinOp oper expr1 expr2 = do
  (o1, u1, i1) <- genExpr expr1
  (o2, u2, i2) <- genExpr expr2
  case (o1, o2) of
    (Imm _, Imm _) -> error ""
    (Mem m1, Imm c2) -> return (Reg EAX, singleUsed EAX,
      [ILoad m1 EAX, IBinOp oper (Imm c2) (Reg EAX)])
    (Imm c1, Mem m2) -> return (Reg EAX, singleUsed EAX,
      if oper == ADD || oper == MUL
        then [ILoad m2 EAX, IBinOp oper (Imm c1) (Reg EAX)]
        else [IMov (Imm c1) (Reg EAX), IBinOp oper (Reg EAX) (Mem m2)])
    (Mem m1, Mem m2) -> return (Reg EAX, singleUsed EAX,
      [ILoad m1 EAX, IBinOp oper (Reg EAX) (Mem m2)])
    (Reg r1, Imm c2) -> return (Reg r1, u1,
      i1 ++ [IBinOp oper (Reg r1) (Imm c2)])
    (Imm c1, Reg r2) -> return $
      if oper == ADD || oper == MUL
        then (Reg r2, u2, i2 ++ [IBinOp oper (Reg r2) (Imm c1)])
      else if r2 == EAX
        then (Reg ECX, setUsed ECX u2,
        i2 ++ [IMov (Imm c1) (Reg ECX), IBinOp oper (Reg ECX) (Reg r2)])
      else (Reg EAX, u2, i2 ++ [IBinOp oper (Reg EAX) (Reg r2)])
    (Reg r1, Reg r2) -> case compare r1 r2 of
      GT -> return (Reg r1, allEmpty, i1 ++ i2 ++ [IBinOp oper (Reg r1) (Reg r2)])
      LT -> return (Reg r2, allEmpty, i2 ++ i1 ++ [IXchg r1 r2, IBinOp oper (Reg r1) (Reg r2)])
      EQ -> return $ if r1 == maxBound
        then let r3 = minBound in
          (Reg r1, allEmpty, i2 ++ [IPush r2] ++ i1 ++ [IPop r3, IBinOp oper (Reg r1) (Reg r3)])
        else let r1 = succ r1 in
          (Reg r1, allEmpty, i1 ++ [IMov (Reg r2) (Reg r1)] ++ i2 ++ [IBinOp oper (Reg r1) (Reg r2)])
