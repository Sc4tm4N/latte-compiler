module Backend.Asm.Utils where

import Prelude hiding (id)
import Control.Monad.State
import qualified Data.Map as M
import Backend.Asm.Data
import Backend.CompilerData

spaceToLocation :: AsmSpace -> Latte(Location)
spaceToLocation (Reg registerIdx) = return $ Register registerIdx
spaceToLocation (Stack i) = return $ MemoryCall (RegisterMemory EBP (Addiction (Const i)))
spaceToLocation (Constant (Num i)) = return $ Const i
spaceToLocation (Constant (Word word)) = do
  store <- get
  return $ ConstString ((stringsEnv store) M.! word)
spaceToLocation (Constant (Log b)) = return $ ConstBool b
spaceToLocation (MemInRegister registerIdx) = return $ MemoryCall (RegisterMemory registerIdx JustCall)

printMov :: AsmSpace -> AsmSpace -> Bool -> Latte([Instruction])
printMov st1@(Stack _) st2@(Stack _) _ = do
  sval1 <- spaceToLocation st1
  movl <- printMov (Reg ECX) st2 False
  return $ movl ++ [MOV sval1 (Register ECX)]
printMov val1@(Stack _) val2@(Reg _) True = do
  sval1 <- spaceToLocation val1
  movl <- printMov (Reg ECX) val2 True
  return $ movl ++ [MOV sval1 (Register ECX)]
printMov val1 val2@(Reg registerIdx) True = do
  sval1 <- spaceToLocation val1
  _ <- spaceToLocation val2
  return $ [MOV sval1 (MemoryCall (RegisterMemory registerIdx JustCall))]
printMov val1 val2 _ = do
  sval1 <- spaceToLocation val1
  sval2 <- spaceToLocation val2
  return $ [MOV sval1 sval2]

stringsToInstructions :: M.Map String String -> [Instruction]
stringsToInstructions stringsMapping = map toInstruction (M.toList stringsMapping) where
  toInstruction (id, label) = STRING_LABEL (label) (".string " ++ id)

impossiblePattern :: String
impossiblePattern = "This haskell pattern is not supposed to be fulfilled at any time. If it somehow does, please contact authors with example."