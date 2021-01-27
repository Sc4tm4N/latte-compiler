module Backend.Asm.CompilerStatement  where

import Prelude hiding (id)
import Control.Monad.State
import qualified Data.Map as M
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Backend.Asm.Data
import Backend.Asm.ConstantParts
import Backend.Asm.Utils
import Backend.Asm.CompilerExpression
import Backend.CompilerData


genStatements :: [Stmt ErrPos] -> [Instruction] -> [Instruction] -> Latte(Bool)
genStatements [] _ _ = return (False)
genStatements (statement:rest) suffix contextUpdate = do
  used <- genStatement statement suffix contextUpdate
  used' <- genStatements rest suffix contextUpdate
  return (used && used')

genStatement :: Stmt ErrPos -> [Instruction] -> [Instruction] -> Latte(Bool)
genStatement (Empty _) _ _ = return (False)
genStatement (BStmt _ (Block _ statements)) suffix contextUpdate = do
  store <- get
  used <- genStatements statements suffix contextUpdate
  store' <- get
  put store' {
    variableEnv = variableEnv store
  }
  return (used)
genStatement (Decl _ _ []) _ _ = return (False)
genStatement (Decl errPos declaredType (item:items)) suffix contextUpdate = do
  case item of
    NoInit _ id -> do
      let expr = case declaredType of {
        (TStr _) -> EString errPos "\"\"";
        (TBool _) -> ELitFalse errPos;
        _ -> ELitInt errPos 0;
      }
      store' <- get
      put $ store' {
        variableEnv = M.insert id (nextVariable store', declaredType) (variableEnv store'),
        nextVariable = (nextVariable store') - 4
      }
      used <- genStatement (Ass errPos (EVar errPos id) expr) [] contextUpdate
      used' <- genStatement (Decl errPos declaredType items) [] contextUpdate
      return (used && used')
    Init _ id expr -> do
      store' <- get
      put $ store' {
        nextVariable = (nextVariable store') - 4
      }
      ((_, _, val), used) <- generateExpression expr suffix contextUpdate
      store'' <- get
      movl <- printMov val (Stack (nextVariable store')) False
      put $ store'' {
        variableEnv = M.insert id (nextVariable store', declaredType) (variableEnv store''),
        generated = movl ++ (generated store'')
      }
      used' <- genStatement (Decl errPos declaredType items) [] contextUpdate
      return (used && used')
genStatement (Ass _ e1 e2) suffix contextUpdate = do
  ((_, _, val2), used) <- generateExpression e2 suffix contextUpdate
  svar2 <- spaceToLocation val2
  store <- get
  put $ store {
    generated = (PUSH svar2):(generated store) }
  ((_, val1, _), used') <- generateExpression e1 suffix contextUpdate
  movl2 <- printMov (Reg EDX) val1 True
  store' <- get
  put $ store' {
    generated = movl2 ++ (POP (Register EDX)):(generated store') }
  _ <- resolveExpressionType e2
  return (used && used')
genStatement (Incr errPos expression) suffix contextUpdate =
  genStatement (Ass errPos expression (EAdd errPos expression (Plus errPos) (ELitInt errPos 1))) suffix contextUpdate
genStatement (Decr errPos expression) suffix contextUpdate =
  genStatement (Ass errPos expression (EAdd errPos expression (Minus errPos) (ELitInt errPos 1))) suffix contextUpdate
genStatement (Ret _ expression) suffix contextUpdate = do
  store <- get
  put $ store {
    generated = suffix ++ (generated store)
  }
  ((_, _, val), used) <- generateExpression expression suffix contextUpdate
  movl <- printMov val (Reg EAX) False
  if used then do
    store' <- get
    put $ store' {
      generated = functionSuffix ++ movl ++ (COMMENT "moving calculated function result to eax"):(generated store')
    }
    return (True)
  else do
    store' <- get
    put $ store' {
      generated = functionSuffix ++ suffix ++ movl ++ (COMMENT "moving calculated function result to eax"):(generated store')
    }
    return (True)
genStatement (VRet _) suffix _ = do
  store <- get
  put $ store {
    generated = functionSuffix ++ (suffix ++ (generated store))
  }
  return (True)
genStatement (Cond pos e stmt) suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 2
  }
  (_, used) <- generateBoolExpression e (labelCount store) ((labelCount store) + 1) suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (LABEL $ ".Label" ++ (show $ labelCount store)):(generated store')
  }
  used' <- genStatement (BStmt pos (Block pos [stmt])) suffix contextUpdate
  store'' <- get
  put $ store'' {
    generated = (LABEL $ ".Label" ++ (show $ (labelCount store) + 1)):(generated store''),
    variableEnv = (variableEnv store)
  }
  return(used && used')
genStatement (CondElse pos e stmt1 stmt2) suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 3
  }
  (_, used) <- generateBoolExpression e (labelCount store) ((labelCount store) + 1) suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (LABEL $ ".Label" ++ (show $ labelCount store)):(generated store')
  }
  used' <- genStatement (BStmt pos (Block pos [stmt1])) suffix contextUpdate
  store'' <- get
  put $ store'' {
    generated = (LABEL $ ".Label" ++ (show $ (labelCount store) + 1))
      :(JMP (Label $ ".Label" ++ (show $ (labelCount store) + 2)))
      :(generated store'') }
  used'' <- genStatement (BStmt pos (Block pos [stmt2])) suffix contextUpdate
  store''' <- get
  put $ store''' {
    generated = (LABEL $ ".Label" ++ (show $ (labelCount store) + 2))
    :(generated store'''),
    variableEnv = (variableEnv store)
  }
  return(used && (used' && used''))
genStatement (While _ e stmt) suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 3,
    generated = (LABEL $ ".Label" ++ (show $ labelCount store)):(generated store)
  }
  (_, used) <- generateBoolExpression e ((labelCount store) + 1) ((labelCount store) + 2) suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (LABEL $ ".Label" ++ (show $ (labelCount store) + 1)):(generated store')
  }
  used' <- genStatement stmt suffix contextUpdate
  store'' <- get
  put $ store'' {
    generated = (LABEL $ ".Label" ++ (show $ (labelCount store) + 2))
    :(JMP $ Label $".Label" ++ (show $ labelCount store)):(generated store''),
    variableEnv = (variableEnv store')
   }
  return (used && used')
genStatement (For errPos t id e statements) suffix contextUpdate = let atArrPos = (EArrAt errPos e (EVar errPos (Ident "for"))) in
  genStatement (BStmt errPos (Block errPos [
    Decl errPos (TInt errPos) [Init errPos (Ident "for") (ELitInt errPos 0)],
    While errPos (ERel errPos (EVar errPos (Ident "for")) (LTH errPos) (EAttrType errPos e (Ident "length")))
      (BStmt errPos (Block errPos [
        Decl errPos t [Init errPos id atArrPos],
        statements,
        Incr errPos (EVar errPos (Ident "for"))]))
  ])) suffix contextUpdate
genStatement (SExp _ expression) suffix contextUpdate = do
  (_, used) <- generateExpression expression suffix contextUpdate
  return (used)
