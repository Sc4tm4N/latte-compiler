module Backend.Asm.Optimisation.CommentEliminator where

import Backend.Asm.Data

eliminateComments :: [Instruction] -> [Instruction]
eliminateComments instructions =
  (_eliminateComments instructions [])

_eliminateComments :: [Instruction] -> [Instruction] -> [Instruction]
_eliminateComments [] accumulator = reverse accumulator
_eliminateComments ((COMMENT _):rest) accumulator =
  _eliminateComments rest accumulator
_eliminateComments (i:rest) accumulator = _eliminateComments rest (i:accumulator)