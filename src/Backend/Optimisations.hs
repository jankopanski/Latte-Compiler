module Backend.Optimisations where

import Backend.IntermediateCode

optimise :: Code -> IO Code
optimise code =
  applyph 1 optMove code >>=
  applyph 1 optAddId >>=
  applyph 1 optMulId >>=
  applyph 3 optJump2 >>=
  applyph 2 optJump1 >>=
  applyph 2 optStore

applyph :: Int -> ([Instruction] -> [Instruction]) -> Code -> IO Code
applyph size opt = return . peephole size opt

peephole :: Int -> ([Instruction] -> [Instruction]) -> Code -> Code
peephole size opt code =
  let optfuns = map (\(ident, ins) -> (ident, iterFun size opt ins)) (functions code) in
  code { functions = optfuns }

iterFun :: Int -> ([Instruction] -> [Instruction]) -> [Instruction] -> [Instruction]
iterFun size opt ins = if length ins < size then ins else
  let (window, ins') = splitAt size ins in
  iterIns size opt window [] ins'

iterIns :: Int -> ([Instruction] -> [Instruction]) ->
  [Instruction] -> [Instruction] -> [Instruction] -> [Instruction]
iterIns size opt window done ins =
  let window' = opt window in
  let n = length window' in
  if n == size then
    if null ins then
      reverse $ reverse window' ++ done
    else
      iterIns size opt (tail window' ++ [head ins]) (head window' : done) (tail ins)
  else if length ins < size - n then
    reverse $ reverse ins ++ reverse window' ++ done
  else
    let (app, ins') = splitAt (size - n) ins in
    iterIns size opt (window' ++ app) done ins'

optMove :: [Instruction] -> [Instruction]
optMove i@[IMov (Reg r1) r2] = if r1 == r2 then [] else i
optMove i = i

optStore :: [Instruction] -> [Instruction]
optStore i@[IStore (Reg r1) m2, ILoad m3 r4] =
  if m2 == m3 then
    if r1 == r4 then [IStore (Reg r1) m2]
    else [IStore (Reg r1) m2, IMov (Reg r1) r4]
  else i
optStore i = i

optAddId :: [Instruction] -> [Instruction]
optAddId i@[IBinOp op _ (Imm 0)] = if op == ADD || op == SUB then [] else i
optAddId i = i

optMulId :: [Instruction] -> [Instruction]
optMulId i@[IBinOp op _ (Imm 1)] = if op == MUL || op == DIV || op == MOD then [] else i
optMulId i = i

optJump1 :: [Instruction] -> [Instruction]
optJump1 i@[IJump l1, ILabel l2] = if l1 == l2 then [ILabel l2] else i
optJump1 i = i

optJump2 :: [Instruction] -> [Instruction]
optJump2 i@[IJumpCond op r1 o2 l1, IJump l2, ILabel l3] =
  if l1 == l3 then [IJumpCond (negRelOp op) r1 o2 l2, IJump l1, ILabel l3] else i
optJump2 i = i
