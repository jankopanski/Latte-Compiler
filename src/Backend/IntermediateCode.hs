module Backend.IntermediateCode where

import Parser.AbsLatte

-- Data structures --

type Size = Integer

data Label = Label Integer | StringLabel Integer deriving (Eq)

data Register = EBP | ESP | EAX | ECX | EDX | EBX | EDI | ESI
  deriving (Eq, Ord, Enum, Bounded)

data Memory
  = MemoryArgument Integer
  | MemoryLocal Integer
  | MemoryOffset Register Integer
  | MemoryGlobal Label
  deriving (Eq)

data Operand
  = Reg Register
  | Mem Memory
  | Imm Integer

data BinaryOperator = ADD | SUB | MUL | DIV | MOD deriving Eq

data RelationOperator = REQ | RNE | RGT | RGE | RLT | RLE deriving Eq

data UnaryOperator = NEG | INC | DEC deriving Eq

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

data Code = Code {
  functions :: [(Ident, [Instruction])],
  strings :: [(String, Label)]
} deriving (Show)

-- Helper functions --

wordLen :: Size
wordLen = 4

mapRelOp :: RelOp () -> RelationOperator
mapRelOp (LTH ()) = RLT
mapRelOp (LE ()) = RLE
mapRelOp (GTH ()) = RGT
mapRelOp (GE ()) = RGE
mapRelOp (EQU ()) = REQ
mapRelOp (NE ()) = RNE

negRelOp :: RelationOperator -> RelationOperator
negRelOp REQ = RNE
negRelOp RNE = REQ
negRelOp RGT = RLE
negRelOp RGE = RLT
negRelOp RLT = RGE
negRelOp RLE = RGT

revRelOp :: RelationOperator -> RelationOperator
revRelOp RGT = RLT
revRelOp RGE = RLE
revRelOp RLT = RGT
revRelOp RLE = RGE
revRelOp oper = oper

-- Show functions --

instance Show Label where
  show (Label n) = ".L" ++ show n
  show (StringLabel n) = ".LC" ++ show n

instance Show Register where
  show EBP = "%ebp"
  show ESP = "%esp"
  show EAX = "%eax"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EBX = "%ebx"
  show EDI = "%edi"
  show ESI = "%esi"

instance Show Memory where
  show (MemoryArgument n) = show n ++ "(" ++ show EBP ++ ")"
  show (MemoryLocal n) = "-" ++ show n ++ "(" ++ show EBP ++ ")"
  show (MemoryOffset r n) = "-" ++ show n ++ "(" ++ show EBP ++ ", " ++
                            show r ++ ", " ++ show wordLen ++ ")"
  show (MemoryGlobal l) = "$" ++ show l

instance Show Operand where
  show (Reg r) = show r
  show (Mem m) = show m
  show (Imm n) = "$" ++ show n

instance Show BinaryOperator where
  show ADD = "addl"
  show SUB = "subl"
  show MUL = "imull"
  show DIV = "idivl"
  show MOD = "idivl"

instance Show RelationOperator where
  show REQ = "je"
  show RNE = "jne"
  show RGT = "jg"
  show RGE = "jge"
  show RLT = "jl"
  show RLE = "jle"

instance Show UnaryOperator where
  show NEG = "negl"
  show INC = "incl"
  show DEC = "decl"

instance Show Instruction where
  show (IMov o1 r2) = showDouble "movl" o1 r2
  show (ILoad m1 r2) = showDouble "movl" m1 r2
  show (IStore o1 m2) = showDouble "movl" o1 m2
  show (IXchg r1 r2) = showDouble "xchgl" r1 r2
  show (IPush r) = showSingle "pushl" r
  show (IPop r) = showSingle "popl" r
  show (IParam o) = showSingle "pushl" o
  show (ICall (Ident s) _) = "\tcall " ++ s
  show (IBinOp DIV _ o) = "\tcdq\n" ++ showSingle (show DIV) o
  show (IBinOp MOD _ o) = "\tcdq\n" ++ showSingle (show MOD) o
  show (IBinOp op r1 o2) = showDouble (show op) o2 r1
  show (IUnOp op o) = showSingle (show op) o
  show (IJump l) = showSingle "jmp" l
  show (IJumpCond op r1 o2 l) =
    case o2 of
      Imm 0 -> showDouble "testl" r1 r1
      _ -> showDouble "cmpl" o2 r1
    ++ "\n" ++ showSingle (show op) l
  show IRet = "\tret"
  show (ILabel l) = show l ++ ":"

nextins :: Show a => a -> String
nextins ins = "\n" ++ show ins

showSingle :: Show a => String -> a -> String
showSingle ins arg = "\t" ++ ins ++ " " ++ show arg

showDouble :: (Show a, Show b) => String -> a -> b -> String
showDouble ins arg1 arg2 = "\t" ++ ins ++ " " ++ show arg1 ++ ", " ++ show arg2
