module Backend.Compiler (compile) where

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExceptT)
import Data.List (intercalate)
import qualified Data.Map as M (empty)
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.Grammar.ShowLatte
import Backend.Asm.Data
import Backend.Asm.ConstantParts (pref)
import Backend.Asm.Utils (stringsToInstructions)
import Backend.Asm.CompilerTopDefs (_compile, gatherFunctions, gatherClasses)
import Backend.Asm.Optimisation.DoubleSuffixEliminator (eliminateDoubleReturns)
import Backend.Asm.Optimisation.CommentEliminator (eliminateComments)
import Backend.Asm.Optimisation.JumpsEliminator (eliminateJumps)
import Backend.Asm.Optimisation.MovEliminator (eliminateMovs)
import Backend.Asm.Optimisation.StackOperationsEliminator (eliminateRedundantPushPop)
import Backend.Asm.Optimisation.DeadCodeEliminator (eliminateDeadCode)
import Backend.Asm.Optimisation.OpMerger (mergeOps)
import Backend.CompilerData
import Backend.Utils (gatherStrings)


compile :: Program ErrPos -> IO(Either String String)
compile (Program _ topDefs) = do
  let stringsDefinitions = gatherStrings topDefs
  let classDefinitions = gatherClasses topDefs
  let functionsDefinitions = gatherFunctions topDefs classDefinitions
  result <- runExceptT (
    runStateT (_compile topDefs)
      Store {
        variableEnv = M.empty,
        functionsEnv = functionsDefinitions,
        classesEnv = classDefinitions,
        stringsEnv = stringsDefinitions,
        usedFunctions = [],
        labelCount = 0,
        nextVariable = -4,
        generated = (reverse $ stringsToInstructions stringsDefinitions) ++ [pref],
        suf = [] })
  case result of
    Left err -> return (Right $ "compiler ended with unexpected exception:\n" ++ err)
    Right (res, _) -> return (Left $ intercalate "\n" (generateFinalCode res))

callOptimizations :: [Instruction] -> [Instruction]
callOptimizations instructions =
  let res = eliminateDoubleReturns instructions in
    let res' = eliminateComments res in
      let res'' = eliminateDeadCode res' in
        let res''' = eliminateMovs res'' in
          let res'''' = eliminateJumps res''' in
            let res''''' = eliminateRedundantPushPop res'''' in
              mergeOps res'''''

generateFinalCode :: [Instruction] -> [String]
generateFinalCode instructions = (reverse (map show $ callOptimizations instructions))