module Backend.Asm.Optimisation.JumpsEliminator where

import Backend.Asm.Data

eliminateJumps :: [Instruction] -> [Instruction]
eliminateJumps instructions =
  (_eliminateJumps instructions [])

_eliminateJumps :: [Instruction] -> [Instruction] -> [Instruction]
_eliminateJumps [] accumulator = reverse accumulator
_eliminateJumps ((LABEL labelId1):(JMP (Label labelId2)):rest) accumulator =
  if labelId1 == labelId2 then _eliminateJumps rest ((LABEL labelId1):accumulator)
  else _eliminateJumps rest ((JMP (Label labelId2)):(LABEL labelId1):accumulator)
_eliminateJumps (i:rest) accumulator = _eliminateJumps rest (i:accumulator)