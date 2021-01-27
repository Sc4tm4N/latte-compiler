module Backend.Asm.Data where

data AsmSpace =
  Constant Value
  | Reg RegisterIdx
  | Stack Integer
  | MemInRegister RegisterIdx
data Value =
  Num Integer
  | Word String
  | Log Bool
data BoolRes =
  SureTrue
  | SureFalse
  | Unsure
  deriving (Eq)

data Instruction
    = LEAVE
    | RET
    | JG Location
    | JGE Location
    | JL Location
    | JLE Location
    | JE Location
    | JNE Location
    | JMP Location
    | POP Location
    | CALL Location
    | NEG Location
    | CMP Location Location
    | SUB Location Location -- how much, from what to what
    | ADD Location Location -- how much, from what to what
    | MUL Location Location
    | MOV Location Location -- from, to
    | SAR Location Location
    | AND Location Location
    | DIV Location
    | PUSH Location
    | BARE String
    | STRING_LABEL String String
    | LABEL String
    | COMMENT String
  deriving (Read, Eq)
  
instance Show Instruction where
  show LEAVE = "  leave"
  show RET = "  ret"
  show (JG l) = "  jg " ++ show l
  show (JGE l) = "  jge " ++ show l
  show (JL l) = "  jl " ++ show l
  show (JLE l) = "  jle " ++ show l
  show (JE l) = "  je " ++ show l
  show (JNE l) = "  jne " ++ show l
  show (JMP l) = "  jmp " ++ show l
  show (POP l) = "  popl " ++ show l
  show (CALL l) = "  call " ++ show l
  show (NEG l) = "  neg " ++ show l
  show (CMP l1 l2) = "  cmpl " ++ show l1 ++ ", " ++ show l2
  show (SUB l1 l2) = "  subl " ++ show l1 ++ ", " ++ show l2
  show (ADD l1 l2) = "  addl " ++ show l1 ++ ", " ++ show l2
  show (MUL l1 l2) = "  imul " ++ show l1 ++ ", " ++ show l2
  show (MOV l1 l2) = "  movl " ++ show l1 ++ ", " ++ show l2
  show (SAR l1 l2) = "  sar " ++ show l1 ++ ", " ++ show l2
  show (AND l1 l2) = "  and " ++ show l1 ++ ", " ++ show l2
  show (DIV l) = "  idivl " ++ show l
  show (PUSH l) = "  pushl " ++ show l
  show (BARE content) = content
  show (STRING_LABEL label content) = label ++ ":\n  " ++ content
  show (LABEL label) = label ++ ":"
  show (COMMENT comment) = "# " ++ comment

data Location
    = Register RegisterIdx
    | MemoryCall MemoryAccess
    | Const Integer
    | ConstString String
    | ConstBool Bool
    | Label String
  deriving (Read, Eq)

instance Show Location where
  show (Register idx) = show idx
  show (MemoryCall memoryAccess) = show memoryAccess
  show (Const integer) = "$" ++ show integer
  show (ConstString label) = "$" ++ label
  show (ConstBool bool) = case bool of
    True -> "TRUE"
    False -> "FALSE"
  show (Label label) = label

data MemoryAccess
    = RegisterMemory RegisterIdx Offset
    deriving (Read, Eq)

instance Show MemoryAccess where
  show (RegisterMemory idx offset) = case offset of
    (Addiction loc) -> case loc of
      (Const integer) -> show integer ++ "(" ++ show idx ++ ")"
      _ -> "-- I GUESS I DO NOT NEED IMPLEMENTATION -- " -- TODO
    (JustCall) -> "(" ++ show idx ++ ")"
    (Asterix integer) -> "*" ++ show integer ++ "(" ++ show idx ++ ")"
    _ -> "-- I GUESS I DO NOT NEED IMPLEMENTATION -- " -- TODO

data Offset
    = Addiction Location
    | Multiplication Location
    | Asterix Integer
    | JustCall
    deriving (Read, Show, Eq)

data RegisterIdx
    = EAX
    | EBX
    | ECX
    | EDX
    | ESP
    | EBP
  deriving (Read, Eq)

instance Show RegisterIdx where
  show EAX = "%eax"
  show EBX = "%ebx"
  show ECX = "%ecx"
  show EDX = "%edx"
  show ESP = "%esp"
  show EBP = "%ebp"

defaultSize :: Integer
defaultSize = 4
