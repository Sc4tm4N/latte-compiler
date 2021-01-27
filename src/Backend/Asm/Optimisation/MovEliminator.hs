module Backend.Asm.Optimisation.MovEliminator where

import Backend.Asm.Data

eliminateMovs :: [Instruction] -> [Instruction]
eliminateMovs instructions =
  (_eliminateMovs instructions [])

_eliminateMovs :: [Instruction] -> [Instruction] -> [Instruction]
_eliminateMovs [] accumulator = reverse accumulator
_eliminateMovs ((MOV loc1 loc2):rest) accumulator =
  if loc1 == loc2 then _eliminateMovs rest accumulator
  else _eliminateMovs rest ((MOV loc1 loc2):accumulator)
_eliminateMovs (i:rest) accumulator = _eliminateMovs rest (i:accumulator)