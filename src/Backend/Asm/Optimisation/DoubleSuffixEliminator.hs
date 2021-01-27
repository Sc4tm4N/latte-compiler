module Backend.Asm.Optimisation.DoubleSuffixEliminator where

import Backend.Asm.Data

eliminateDoubleReturns :: [Instruction] -> [Instruction]
eliminateDoubleReturns instructions =
  (_eliminateDoubleReturns instructions [] False)

_eliminateDoubleReturns :: [Instruction] -> [Instruction] -> Bool -> [Instruction]
_eliminateDoubleReturns [] accumulator _ = reverse accumulator
_eliminateDoubleReturns ((LABEL label):rest) accumulator _ =
  _eliminateDoubleReturns rest ((LABEL label):accumulator) False
_eliminateDoubleReturns (RET:LEAVE:(COMMENT _):rest) accumulator True = _eliminateDoubleReturns rest accumulator True
_eliminateDoubleReturns (RET:LEAVE:rest) accumulator False = _eliminateDoubleReturns rest (LEAVE:RET:accumulator) True
_eliminateDoubleReturns (instruction:rest) accumulator suffixActive =
  _eliminateDoubleReturns rest (instruction:accumulator) suffixActive

-- 0 move, 1 skip but met, 2 cut
eliminateDoubleContextUpdates :: [Instruction] -> [Instruction] -> Integer -> [Instruction]
eliminateDoubleContextUpdates [] accumulator _ = accumulator
eliminateDoubleContextUpdates ((LABEL label):rest) accumulator _ =
  eliminateDoubleContextUpdates rest ((LABEL label):accumulator) 0
eliminateDoubleContextUpdates ((COMMENT "UPDATE MARKER - BEG"):rest) accumulator 0 =
  eliminateDoubleContextUpdates rest accumulator 1
eliminateDoubleContextUpdates ((COMMENT "UPDATE MARKER - BEG"):rest) accumulator 1 =
  eliminateDoubleContextUpdates rest accumulator 2
eliminateDoubleContextUpdates ((COMMENT "UPDATE MARKER - END"):rest) accumulator 1 =
  eliminateDoubleContextUpdates rest accumulator 1
eliminateDoubleContextUpdates ((COMMENT "UPDATE MARKER - END"):rest) accumulator 2 =
  eliminateDoubleContextUpdates rest accumulator 1
eliminateDoubleContextUpdates (any:rest) accumulator 0 =
  eliminateDoubleContextUpdates rest (any:accumulator) 0
eliminateDoubleContextUpdates (any:rest) accumulator 1 =
  eliminateDoubleContextUpdates rest (any:accumulator) 1
eliminateDoubleContextUpdates (any:rest) accumulator 2 =
  eliminateDoubleContextUpdates rest accumulator 2