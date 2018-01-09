module Backend.Optimisations where

import Backend.IntermediateCode

optimise :: Code -> IO Code
optimise code = return (peephole 2 optMove code)

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
-- iterIns _ _ window done [] = reverse $ reverse window ++ done
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
    -- iterIns size opt ins (reverse window' ++ done) []
  else
    let (app, ins') = splitAt (size - n) ins in
    iterIns size opt (window' ++ app) done ins'

optMove :: [Instruction] -> [Instruction]
optMove i@[IMov (Reg r1) r2] = if r1 == r2 then [] else i
optMove i = i

-- optStore :: [Instruction] -> [Instruction]
-- optStore
-- optStore
