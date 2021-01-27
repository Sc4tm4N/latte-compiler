module Backend.Asm.Optimisation.StackOperationsEliminator where

import Backend.Asm.Data

eliminateRedundantPushPop :: [Instruction] -> [Instruction]
eliminateRedundantPushPop instructions =
  case (_eliminateRedundantPushPop instructions [] False) of
    (cleaned', False) -> cleaned'
    (cleaned', True) -> eliminateRedundantPushPop cleaned'

_eliminateRedundantPushPop :: [Instruction] -> [Instruction] -> Bool -> ([Instruction], Bool)
_eliminateRedundantPushPop [] accumulator anyRemoved = ((reverse accumulator), anyRemoved)
_eliminateRedundantPushPop ((POP loc1):(PUSH loc2):rest) accumulator anyRemoved =
  if loc1 == loc2 then _eliminateRedundantPushPop rest accumulator True
  else (
    case (loc1, loc2) of
      (Register _, Register _) -> _eliminateRedundantPushPop rest ((MOV loc2 loc1):accumulator) True
      (MemoryCall _, Register _) -> _eliminateRedundantPushPop rest ((MOV loc2 loc1):accumulator) True
      (Const _, Register _) -> _eliminateRedundantPushPop rest ((MOV loc2 loc1):accumulator) True
      (ConstString _, Register _) -> _eliminateRedundantPushPop rest ((MOV loc2 loc1):accumulator) True
      _ -> _eliminateRedundantPushPop rest ((PUSH loc2):(POP loc1):accumulator) anyRemoved
  )
_eliminateRedundantPushPop ((PUSH loc1):(POP loc2):rest) accumulator anyRemoved =
  if loc1 == loc2 then _eliminateRedundantPushPop rest accumulator True
  else _eliminateRedundantPushPop rest ((POP loc2):(PUSH loc1):accumulator) anyRemoved
_eliminateRedundantPushPop (i:rest) accumulator anyRemoved = _eliminateRedundantPushPop rest (i:accumulator) anyRemoved
