module Backend.Asm.ExternalFunctionCalls where

import Backend.Asm.Data

calloc :: Location -> [Instruction]
calloc location = [
  (ADD (Const 16) (Register ESP)),
  (CALL (Label "calloc")),
  (PUSH (Register EAX)),
  (ADD (Const 1) (Register EAX)),
  (MOV location (Register EAX)),
  (PUSH (Const defaultSize)),
  (SUB (Const 8) (Register ESP)),
  (COMMENT "calloc function call with stack aligning")]

memcpy :: Location -> Location -> Integer -> [Instruction]
memcpy src dest i = [
  (ADD (Const 16) (Register ESP)),
  (CALL (Label "memcpy")),
  (PUSH dest),
  (PUSH src),
  (PUSH (Const i)),
  (SUB (Const 4) (Register ESP)),
  (COMMENT "memcpy function call with stack aligning")]

strcmp :: [Instruction]
strcmp = [
  (ADD (Const 16) (Register ESP)),
  (CALL (Label "strcmp")),
  (PUSH (Register EAX)),
  (PUSH (Register EDX)),
  (SUB (Const 8) (Register ESP)),
  (COMMENT "strcmp function call with stack aligning")]