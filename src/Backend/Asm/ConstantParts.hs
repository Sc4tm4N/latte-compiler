module Backend.Asm.ConstantParts where

import Backend.Asm.Data

pref :: Instruction
pref = BARE ".globl main"

functionPrefix :: [Instruction]
functionPrefix = [AND (Const $ negate 16) (Register ESP), MOV (Register ESP) (Register EBP), PUSH (Register EBP), COMMENT "function prefix, with stack alignment"]

functionSuffix :: [Instruction]
functionSuffix = [RET, LEAVE, COMMENT "function suffix"]