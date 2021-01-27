module Backend.Asm.Optimisation.OpMerger where

import Backend.Asm.Data

mergeOps :: [Instruction] -> [Instruction]
mergeOps instructions =
  _mergeOps instructions []

_mergeOps :: [Instruction] -> [Instruction] -> [Instruction]
_mergeOps [] accumulator = reverse accumulator
_mergeOps ((ADD (Const howMuch1) loc1):(ADD (Const howMuch2) loc2):rest) accumulator  =
  if loc1 == loc2 then _mergeOps ((ADD (Const $ howMuch1 + howMuch2) loc1):rest) accumulator
  else _mergeOps rest ((ADD (Const howMuch2) loc2):(ADD (Const howMuch1) loc1):accumulator)
_mergeOps ((SUB (Const howMuch1) loc1):(SUB (Const howMuch2) loc2):rest) accumulator  =
  if loc1 == loc2 then _mergeOps ((SUB (Const $ howMuch1 + howMuch2) loc1):rest) accumulator
  else _mergeOps rest ((SUB (Const howMuch2) loc2):(SUB (Const howMuch1) loc1):accumulator)
_mergeOps ((SUB (Const howMuch1) loc1):(ADD (Const howMuch2) loc2):rest) accumulator  =
  if loc1 == loc2 then _mergeOps ((if howMuch2 > howMuch1 then (ADD (Const $ howMuch2 - howMuch1) loc1) else (SUB (Const $ howMuch1 - howMuch2) loc1)):rest) accumulator
  else _mergeOps rest ((ADD (Const howMuch2) loc2):(SUB (Const howMuch1) loc1):accumulator)
_mergeOps ((ADD (Const howMuch1) loc1):(SUB (Const howMuch2) loc2):rest) accumulator  =
  if loc1 == loc2 then _mergeOps ((if howMuch2 > howMuch1 then (SUB (Const $ howMuch2 - howMuch1) loc1) else (ADD (Const $ howMuch1 - howMuch2) loc1)):rest) accumulator
  else _mergeOps rest ((SUB (Const howMuch2) loc2):(ADD (Const howMuch1) loc1):accumulator)
_mergeOps (i:rest) accumulator  = _mergeOps rest (i:accumulator)
