module Backend.Asm.Optimisation.DeadCodeEliminator where

import Backend.Asm.Data

eliminateDeadCode :: [Instruction] -> [Instruction]
eliminateDeadCode instructions =
  (_eliminateDeadCode (reverse instructions) [] False)

_eliminateDeadCode :: [Instruction] -> [Instruction] -> Bool -> [Instruction]
_eliminateDeadCode [] accumulator _ = accumulator
_eliminateDeadCode ((LABEL label):rest) accumulator True  =
  _eliminateDeadCode rest ((LABEL label):accumulator) False
_eliminateDeadCode (_:rest) accumulator True  =
  _eliminateDeadCode rest accumulator True
_eliminateDeadCode (RET:rest) accumulator False  =
  _eliminateDeadCode rest (RET:accumulator) True
_eliminateDeadCode ((JMP loc):rest) accumulator False  =
  _eliminateDeadCode rest ((JMP loc):accumulator) True
_eliminateDeadCode (i:rest) accumulator False = _eliminateDeadCode rest (i:accumulator) False