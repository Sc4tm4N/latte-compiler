module Backend.Asm.CompilerExpression  where

import Prelude hiding (id)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import qualified Data.Map as M
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Backend.Asm.Data
import Backend.Asm.ExternalFunctionCalls
import Backend.Asm.Utils
import Backend.CompilerData


resolveExpressionType :: Expr ErrPos -> Latte(Type ErrPos)
resolveExpressionType (EVar _ (Ident id)) = do
  store <- get
  if M.member (Ident id) (variableEnv store) then do
    let res = snd ((variableEnv store) M.! (Ident id))
    return res
  else do
    return $ snd ((variableEnv store) M.! (Ident $ "self+" ++ id))
resolveExpressionType (ELitInt errPos _) = return (TInt errPos)
resolveExpressionType (ELitTrue errPos) = return (TBool errPos)
resolveExpressionType (ELitFalse errPos) = return (TBool errPos)
resolveExpressionType (EApp errPos (Ident id) arguments) = do
  store <- get
  if M.member (Ident "self") (variableEnv store) then do
    let (TClass _ classId) = snd ((variableEnv store) M.! (Ident "self"))
    if M.member (Ident id) (functionsEnv store) then do
      return(snd ((functionsEnv store) M.! (Ident id)))
    else resolveExpressionType (EAttrFun errPos (EVar errPos (Ident "self")) (Ident id) arguments)
  else do
    return(snd ((functionsEnv store) M.! (Ident id)))
resolveExpressionType (EString errPos _) = return (TStr errPos)
resolveExpressionType (ENewArr errPos arrayType _) = return (TTab errPos arrayType)
resolveExpressionType (ENewClass _ classType) = return classType
resolveExpressionType (EAttrFun _ classExpression functionId _) = do
  (TClass _ classId) <- resolveExpressionType classExpression
  resolveParentFunction classId functionId
resolveExpressionType (EAttrType errPos classExpression attributeId@(Ident id)) =
  if classExpression == EVar errPos (Ident "self") then
    resolveExpressionType (EVar errPos (Ident $ "self+" ++ id))
  else do
    realType <- resolveExpressionType classExpression
    case realType of
      (TClass _ classId) -> do
        store <- get
        let (_, _, attributes,_) = (classesEnv store) M.! classId
        let (ClassType _, attributeType) = attributes M.! (attributeId, DataAttribute)
        return attributeType
      (TTab _ _) -> return (TInt errPos)
      _ -> throwError $ impossiblePattern
resolveExpressionType (EArrAt _ expression _) = do
  (TTab _ arrayType) <- resolveExpressionType expression
  return arrayType
resolveExpressionType (ENullCast errPos classId) = return (TClass errPos classId)
resolveExpressionType (ECast errPos classId _) = return (TClass errPos classId)
resolveExpressionType (Neg errPos _) = return (TInt errPos)
resolveExpressionType (Not errPos _) = return (TBool errPos)
resolveExpressionType (EMul errPos _ _ _) = return (TInt errPos)
resolveExpressionType (EAdd _ expr _ _) = resolveExpressionType expr
resolveExpressionType (ERel errPos _ _ _ ) = return (TBool errPos)
resolveExpressionType (EAnd errPos _ _) = return (TBool errPos)
resolveExpressionType (EOr errPos _ _) = return (TBool errPos)


resolveParentFunction :: Ident -> Ident -> Latte (Type ErrPos)
resolveParentFunction classId functionId = do
  store <- get
  let (parent, _, attributes,_) = (classesEnv store) M.! classId
  if M.member (functionId, FunctionAttribute) attributes then do
    let (ClassFunction _, returnType) = attributes M.! (functionId, FunctionAttribute)
    return returnType
  else
    case parent of
      Nothing -> throwError $ impossiblePattern
      Just id -> resolveParentFunction id functionId

generateExpression :: Expr ErrPos -> [Instruction] -> [Instruction] -> Latte ((AsmSpace, AsmSpace, AsmSpace), Bool)
generateExpression (EVar _ (Ident id)) _ _ = do
  store <- get
  if M.member (Ident id) (variableEnv store) then do
    let result = Stack $ fst ((variableEnv store) M.! (Ident id))
    return ((result, result, result), False)
  else do
    let result = Stack $ fst ((variableEnv store) M.! (Ident $ "self+" ++ id))
    return ((result, result, result), False)
generateExpression (ELitInt _ integer) _ _ =
  return ((Constant $ Num integer, Constant $ Num integer, Constant $ Num integer), False)
generateExpression (EApp errPos (Ident id) arguments) suffix contextUpdate = do
  store <- get
  if M.member (Ident "self") (variableEnv store) then do
    let (TClass _ classId) = snd ((variableEnv store) M.! (Ident "self"))
    if M.member (Ident id) (functionsEnv store) then generateFunctionCall id arguments Nothing suffix contextUpdate
    else generateExpression (EAttrFun errPos (EVar errPos (Ident "self")) (Ident id) arguments) suffix contextUpdate
  else generateFunctionCall id arguments Nothing suffix contextUpdate
generateExpression (EString _ string) _ _ =
  return ((Constant $ Word string, Constant $ Word string, Constant $ Word string), False)
generateExpression (ENewArr _ _ sizeExpression) suffix contextUpdate = do
  ((_, _, space'), used) <- generateExpression sizeExpression suffix contextUpdate
  store <- get
  location <- spaceToLocation space'
  put $ store {
    generated = (MOV (Register EDX) (MemoryCall (RegisterMemory EAX JustCall)))
    :(POP (Register EDX))
    :((calloc location) ++ ((PUSH location):(generated store))) }
  return ((Reg EAX, Reg EAX, Reg EAX), used)
generateExpression (ENewClass _ (TClass _ classId)) _ _ = do
  store <- get
  let (_, i, _, _) = (classesEnv store) M.! classId
  location <- spaceToLocation $ Constant $ Num (div i defaultSize)
  put $ store {
    generated = (MOV (Register EDX) (MemoryCall (RegisterMemory EAX JustCall)))
    :(MOV (ConstString $ "." ++ show classId ++ "Functions") (Register EDX))
    :((calloc location) ++ (generated store)) }
  return $ ((Reg EAX, Reg EAX, Reg EAX), False)
generateExpression (EAttrFun _ expression (Ident id) expressions) suffix contextUpdate = do
  (TClass _ classId) <- resolveExpressionType expression
  (_, i) <- resolveFunctionNumber classId (Ident id)
  generateFunctionCall id (expression:expressions) (Just i) suffix contextUpdate -- restore context afterwards
generateExpression (EAttrType errPos structureExpression (Ident id)) suffix contextUpdate =
  if structureExpression == EVar errPos (Ident "self") then
    generateExpression (EVar errPos (Ident $ "self+" ++ id)) suffix contextUpdate
  else do
    typeOfExpression <- resolveExpressionType structureExpression
    case typeOfExpression of
      (TClass _ classId) -> do
        ((_, _, classPosition), used) <- generateExpression structureExpression suffix contextUpdate
        location <- spaceToLocation classPosition
        store <- get
        let (_, _, attrs, _) = (classesEnv store) M.! classId
        let (ClassType i, _) = attrs M.! (Ident id, DataAttribute)
        put $ store {
          generated = (PUSH location):(generated store)
        }
        store' <- get
        put $ store' {
          generated = (MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register EDX))
            :(MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register ECX))
            :(ADD (Const i) (Register EAX))
            :(POP (Register EAX))
            :(generated store')
        }
        return $ ((Reg ECX, Reg EAX, Reg EDX), used)
      (TTab _ _) -> do
        ((tabPos, _, _), used) <- generateExpression structureExpression suffix contextUpdate
        location <- spaceToLocation tabPos
        store <- get
        put $ store {
          generated = (MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register EDX))
            :(MOV location (Register EAX))
            :(generated store)
        }
        return $ ((Reg EAX, Reg EAX, Reg EDX), used)
      _ -> throwError $ impossiblePattern
generateExpression (EArrAt _ arrayExpression atExpression) suffix contextUpdate = do
  ((arrPos, _, _), used) <- generateExpression arrayExpression suffix contextUpdate
  location <- spaceToLocation arrPos
  store <- get
  put $ store {
    generated = (PUSH location):(generated store)
  }
  ((_, _, at), used') <- generateExpression atExpression suffix contextUpdate
  movl <- printMov at (Reg EDX) False
  store' <- get
  put $ store' {
    generated = [
      MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register EDX),
      ADD (Register EDX) (Register EAX),
      POP (Register EDX),
      BARE "  leal (, %edx, 4), %eax", -- TODO fix
      ADD (Const 1) (Register EDX)
    ]
    ++ movl
    ++ (generated store') }
  return $ ((Reg EAX, Reg EAX, Reg EDX), used && used')
generateExpression (ENullCast _ _) _ _ = do
  return $ ((Constant $ Num 0, Constant $ Num 0, Constant $ Num 0), False)
generateExpression (ECast _ classId classExpression) suffix contextUpdate = generateExpression classExpression suffix contextUpdate
generateExpression (Neg _ expression) suffix contextUpdate = do
  ((_, _, val), used) <- generateExpression expression suffix contextUpdate
  store <- get
  movl <- printMov val (Reg EAX) False
  put $ store {
    generated = (NEG (Register EAX)):movl ++ (generated store) }
  return ((Reg EAX, Reg EAX, Reg EAX), used)
generateExpression (EMul _ e1 op e2) suffix contextUpdate = case op of
  (Times _) -> generateMultiplyExpression e1 e2 suffix contextUpdate
  (Div _) -> do
    (res, used) <- (generateDivisionExpression e1 e2 suffix contextUpdate)
    return ((fst res, fst res, fst res), used)
  (Mod _) -> do
    (res, used) <- (generateDivisionExpression e1 e2 suffix contextUpdate)
    return ((snd res, snd res, snd res), used)
generateExpression (EAdd errPos e1 operation e2) suffix contextUpdate = case operation of
  (Plus _) -> do
    expressionType1 <- resolveExpressionType e1
    case expressionType1 of
      (TStr _) -> do
        generateExpression (EApp errPos (Ident "concat") [e1,e2]) suffix contextUpdate
      _ -> generatePlusMinusExpression e1 e2 operation suffix contextUpdate
  (Minus _) -> generatePlusMinusExpression e1 e2 operation suffix contextUpdate
generateExpression bExp suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 2
  }
  (_, used) <- generateBoolExpression bExp (labelCount store) ((labelCount store) + 1) suffix contextUpdate
  store' <- get
  movl1 <- printMov (Constant (Num 0)) (Reg EAX) False
  movl2 <- printMov (Constant (Num 1)) (Reg EAX) False
  put $ store' {
    labelCount = (labelCount store') + 1,
    generated =
      (LABEL $ ".Label" ++ (show $ labelCount store')):movl1
      ++ [(LABEL $ ".Label" ++ (show $ (labelCount store) + 1)),
          (JMP $ Label (".Label" ++ (show $ labelCount store')))]
      ++ movl2
      ++ (LABEL $ ".Label" ++ (show $ labelCount store)):(generated store') }
  return ((Reg EAX, Reg EAX, Reg EAX), used)


generateBoolComparison :: (AsmSpace, RelOp ErrPos, AsmSpace) -> Integer -> Integer -> Latte()
generateBoolComparison (val1, op, val2) labelForTrue labelForFalse = do
  let jmp = case op of {
    (LTH _) -> JG (Label $ ".Label" ++ (show labelForTrue));
    (LE _) -> JGE (Label $ ".Label" ++ (show labelForTrue));
    (GTH _) -> JL (Label $ ".Label" ++ (show labelForTrue));
    (GE _) -> JLE (Label $ ".Label" ++ (show labelForTrue));
    (EQU _) -> JE (Label $ ".Label" ++ (show labelForTrue));
    (NE _) -> JNE (Label $ ".Label" ++ (show labelForTrue));
  }
  store <- get
  location <- spaceToLocation val1
  location' <- spaceToLocation val2
  movl <- printMov val2 (Reg EBX) False
  let (addMov, sndCmp) = case val2 of {
    (Constant _) -> (movl, Register EBX);
    (Stack _) -> (movl, Register EBX);
    _ -> ([], location') }
  put $ store {
    generated =
      (JMP (Label $ ".Label" ++ (show labelForFalse)))
      :jmp
      :(CMP location sndCmp)
      :addMov
      ++ (generated store)
  }
  return ()


generateBoolExpression :: Expr ErrPos -> Integer -> Integer -> [Instruction] -> [Instruction] -> Latte((BoolRes, Bool))
generateBoolExpression (EVar errPos (Ident id)) labelForTrue labelForFalse _ _ = do
  store <- get
  if M.member (Ident id) (variableEnv store) then do
    generateBoolComparison (Stack $ fst ((variableEnv store) M.! (Ident id)), EQU errPos, Constant (Num 1)) labelForTrue labelForFalse
    return (Unsure, False)
  else do
    generateBoolComparison (Stack $ fst ((variableEnv store) M.! (Ident $ "self+" ++ id)), EQU errPos, Constant (Num 1)) labelForTrue labelForFalse
    return (Unsure, False)
generateBoolExpression (ELitTrue _) labelForTrue _ _ _ = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 1,
    generated = (JMP $ Label $ ".Label" ++ (show labelForTrue)):(generated store) }
  return (SureTrue, False)
generateBoolExpression (ELitFalse _) _ labelForFalse _ _ = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 1,
    generated = (JMP $ Label $ ".Label" ++ (show labelForFalse)):(generated store) }
  return (SureFalse, False)
generateBoolExpression (EAnd _ e1 e2) labelForTrue labelForFalse suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 1
  }
  (res1, used) <- generateBoolExpression e1 (labelCount store) labelForFalse suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (LABEL (".Label" ++ (show $ labelCount store))):(generated store')
  }
  (res2, used') <- generateBoolExpression e2 labelForTrue labelForFalse suffix contextUpdate
  case (res1, res2) of
    (SureTrue, SureTrue) -> return (SureTrue, used && used')
    (SureFalse, _) -> return (SureFalse, used && used')
    (_, SureFalse) -> return (SureFalse, used && used')
    (_, _) -> return (Unsure, used && used')
generateBoolExpression (EOr _ e1 e2) labelForTrue labelForFalse suffix contextUpdate = do
  store <- get
  put $ store {
    labelCount = (labelCount store) + 1
  }
  (res1, used) <- generateBoolExpression e1 labelForTrue (labelCount store) suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (LABEL (".Label" ++ (show $ labelCount store))):(generated store')
  }
  if (res1 == SureTrue) then
    return (SureTrue, used)
  else do
    (res2, used') <- generateBoolExpression e2 labelForTrue labelForFalse suffix contextUpdate
    case (res1, res2) of
      (SureFalse, SureFalse) -> return (SureFalse, used && used')
      (SureTrue, _) -> return (SureTrue, used && used')
      (_, SureTrue) -> return (SureTrue, used && used')
      (_, _) -> return (Unsure, used && used')
generateBoolExpression app@(EApp pos _ _) labelForTrue labelForFalse suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression app suffix contextUpdate
  case val1 of
    Constant (Log True) -> do
      (be, used') <- generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    Constant (Log False) -> do
      (be, used') <- generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    v -> do
      generateBoolComparison (v, EQU pos, Constant (Num 1)) labelForTrue labelForFalse
      return (Unsure, used)
generateBoolExpression attrFun@(EAttrFun pos _ _ _) labelForTrue labelForFalse suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression attrFun suffix contextUpdate
  case val1 of
    Constant (Log True) -> do
      (be, used') <- generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    Constant (Log False) -> do
      (be, used') <- generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    v -> do
      generateBoolComparison (v, EQU pos, Constant (Num 1)) labelForTrue labelForFalse
      return (Unsure, used)
generateBoolExpression attr@(EAttrType pos _ _) labelForTrue labelForFalse suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression attr suffix contextUpdate
  case val1 of
    Constant (Log True) -> do
      (be, used') <- generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    Constant (Log False) -> do
      (be, used') <- generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
      return (be, used && used')
    v -> do
      generateBoolComparison (v, EQU pos, Constant (Num 1)) labelForTrue labelForFalse
      return (Unsure, used)
generateBoolExpression (Not _ e) labelForTrue labelForFalse suffix contextUpdate = do
  (res, used) <- generateBoolExpression e labelForFalse labelForTrue suffix contextUpdate
  case res of
    SureTrue -> return (SureFalse, used)
    SureFalse -> return (SureTrue, used)
    Unsure -> return (Unsure, used)
generateBoolExpression (ERel pos e1 op e2) labelForTrue labelForFalse suffix contextUpdate = do
  let opFun = relationOperationToHaskellFunction op
  case (e1, e2) of
    (ELitInt _ i, ELitInt _ j) -> if opFun i j then
        generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      else generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
    (ELitTrue _, ELitTrue _) -> if opFun 1 1 then
        generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      else generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
    (ELitTrue _, ELitFalse _) -> if opFun 1 0 then
        generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      else generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
    (ELitFalse _, ELitTrue _) -> if opFun 0 1 then
        generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      else generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
    (ELitFalse _, ELitFalse _) -> if opFun 0 0 then
        generateBoolExpression (ELitTrue pos) labelForTrue labelForFalse suffix contextUpdate
      else generateBoolExpression (ELitFalse pos) labelForTrue labelForFalse suffix contextUpdate
    (_, _) -> do
      ((_, _, val1), used) <- generateExpression e1 suffix contextUpdate
      sval1 <- spaceToLocation val1
      store <- get
      put $ store {
        generated = (PUSH sval1):(generated store) }
      ((_, _, val2), used') <- generateExpression e2 suffix contextUpdate
      movl <- printMov val2 (Reg EDX) False
      store' <- get
      put $ store' {
        generated = (POP (Register EAX)):movl ++ (generated store')
      }
      store'' <- get
      eType <- resolveExpressionType e1
      case eType of
        (TStr _) -> do
          put $ store'' {
            generated = strcmp ++ (generated store'')
          }
          generateBoolComparison (Reg EAX, op, Constant (Num 0)) labelForTrue labelForFalse
          return (Unsure, used && used')
        _ -> do
          generateBoolComparison (Reg EAX, op, Reg EDX) labelForTrue labelForFalse
          return (Unsure, used && used')
generateBoolExpression _ _ _ _ _ = throwError $ impossiblePattern


relationOperationToHaskellFunction :: (Ord a) => RelOp ErrPos -> (a -> a -> Bool)
relationOperationToHaskellFunction (LTH _) = (<)
relationOperationToHaskellFunction (LE _)  = (<=)
relationOperationToHaskellFunction (GTH _) = (>)
relationOperationToHaskellFunction (GE _)  = (>=)
relationOperationToHaskellFunction (EQU _) = (==)
relationOperationToHaskellFunction (NE _)  = (/=)


generateDivisionExpression :: Expr ErrPos -> Expr ErrPos -> [Instruction] -> [Instruction] -> Latte ((AsmSpace, AsmSpace), Bool)
generateDivisionExpression e1 e2 suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression e1 suffix contextUpdate
  sval1 <- spaceToLocation val1
  store <- get
  put $ store {
    generated = (PUSH sval1):(generated store)
  }
  ((_, _, val2), used') <- generateExpression e2 suffix contextUpdate
  store' <- get
  put $ store' {
    generated = (POP (Register ECX)):(generated store')
  }
  store'' <- get
  movl1 <- printMov val2 (Reg EBX) False
  movl2 <- printMov (Reg EAX) (Reg EDX) False
  movl3 <- printMov (Reg ECX) (Reg EAX) False
  put $ store'' {
    generated =
      (DIV (Register EBX)):(SAR (Const 31) (Register EDX)):movl2
      ++ movl3
      ++ movl1
      ++ (generated store'') }
  return $ ((Reg EAX, Reg EDX), used && used')


generatePlusMinusExpression :: Expr ErrPos -> Expr ErrPos -> AddOp ErrPos -> [Instruction] -> [Instruction] -> Latte ((AsmSpace, AsmSpace, AsmSpace), Bool)
generatePlusMinusExpression e1 e2 op suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression e1 suffix contextUpdate
  sval1 <- spaceToLocation val1
  store <- get
  put $ store {
    generated = (PUSH sval1):(generated store)
  }
  ((_, _, val2), used') <- generateExpression e2 suffix contextUpdate
  store' <- get
  movl2 <- printMov val2 (Reg ECX) False
  case op of
    (Plus _) -> do
      put $ store' {
        generated = [(ADD (Register ECX) (Register EAX)), (POP (Register EAX))]
          ++ movl2
          ++ (generated store') }
      return $ ((Reg EAX, Reg EAX, Reg EAX), used && used')
    _ -> do
      put $ store' {
        generated = [(SUB (Register ECX) (Register EAX)), (POP (Register EAX))]
          ++ movl2
          ++ (generated store') }
      return $ ((Reg EAX, Reg EAX, Reg EAX), used && used')

generateMultiplyExpression :: Expr ErrPos -> Expr ErrPos -> [Instruction] -> [Instruction] -> Latte ((AsmSpace, AsmSpace, AsmSpace), Bool)
generateMultiplyExpression e1 e2 suffix contextUpdate = do
  ((_, _, val1), used) <- generateExpression e1 suffix contextUpdate
  sval1 <- spaceToLocation val1
  store <- get
  put $ store {
    generated = (PUSH sval1):(generated store)
  }
  ((_, _, val2), used') <- generateExpression e2 suffix contextUpdate
  store' <- get
  movl2 <- printMov val2 (Reg ECX) False
  put $ store' {
    generated = [(MUL (Register ECX) (Register EAX)), (POP (Register EAX))]
      ++ movl2
      ++ (generated store') }
  return $ ((Reg EAX, Reg EAX, Reg EAX), used && used')

moveParametersImpl :: Bool -> [Expr ErrPos] -> [Instruction] -> Latte()
moveParametersImpl _ [] _ = return ()
moveParametersImpl True (expression:rest) contextUpdate = do
  ((val, _, _), _) <- generateExpression expression [] contextUpdate
  store <- get
  location <- spaceToLocation val
  put $ store {
    generated = (PUSH location):(generated store)
  }
  moveParametersImpl False rest contextUpdate
moveParametersImpl _ (expression:rest) contextUpdate = do
    ((_, _, val), _) <- generateExpression expression [] contextUpdate
    store <- get
    location <- spaceToLocation val
    put $ store {
      generated = (PUSH location):(generated store)
    }
    moveParametersImpl False rest contextUpdate

moveParameters :: Bool -> [Expr ErrPos] -> [Instruction] -> Latte()
moveParameters b expressions contextUpdate = do
  let parametersAmount = toInteger $ length expressions
  let mod4ParametersAmount = (parametersAmount `mod` 4)
  let toSupply = if mod4ParametersAmount == 0 then 0 else (4 - (parametersAmount `mod` 4))
  let mocked = [SUB (Const $ toSupply * defaultSize) (Register ESP)] -- replicate (fromInteger toSupply) (PUSH (Const 0))
  store <- get
  put $ store {
    generated = mocked ++ (generated store)
  }
  moveParametersImpl b expressions contextUpdate


generateFunctionCall :: String -> [Expr ErrPos] -> Maybe Integer -> [Instruction] -> [Instruction] -> Latte ((AsmSpace, AsmSpace, AsmSpace), Bool)
generateFunctionCall _ [] (Just _) _ _ = throwError $ impossiblePattern
generateFunctionCall functionId expressions@(expression:_) (Just i) suffix contextUpdate = do
  store <- get
  put $ store {
    generated = (COMMENT ("potentially virtual call with argument calculations to " ++ show functionId)):suffix ++ (generated store)
  }
  moveParameters True (reverse expressions) contextUpdate
  let exprToClass = (EVar )
  ((classId, _, _), used) <- generateExpression expression suffix contextUpdate
  movl <- printMov classId (Reg EAX) False
  store' <- get
  let parametersAmount = toInteger $ length expressions
  let mod4ParametersAmount = (parametersAmount `mod` 4)
  let popAmount = parametersAmount + if mod4ParametersAmount == 0 then 0 else 4 - mod4ParametersAmount
  let pop = [ADD (Const $ popAmount * defaultSize) (Register ESP), (COMMENT "clean stack after call")]
  put $ store' {
    usedFunctions = functionId:(usedFunctions store'),
    generated =
      contextUpdate
      ++ pop
      ++ [CALL (MemoryCall (RegisterMemory EAX (Asterix $ defaultSize * i)))]
      ++ [MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register EAX)]
      ++ movl
      ++ (generated store')
  }
  return ((Reg EAX, Reg EAX, Reg EAX), used)
generateFunctionCall functionId expressions Nothing suffix contextUpdate = do
  store <- get
  put $ store {
    generated = (COMMENT ("pure call with argument calculations to " ++ show functionId)):suffix ++ (generated store)
  }
  moveParameters False (reverse expressions) contextUpdate
  store' <- get
  let parametersAmount = toInteger $ length expressions
  let mod4ParametersAmount = (parametersAmount `mod` 4)
  let popAmount = parametersAmount + if mod4ParametersAmount == 0 then 0 else 4 - mod4ParametersAmount
  let pop = [ADD (Const $ popAmount * defaultSize) (Register ESP)]
  put $ store' {
    usedFunctions = functionId:(usedFunctions store'),
    generated = contextUpdate ++ pop ++ [CALL (Label functionId)] ++ (generated store')
  }
  return ((Reg EAX, Reg EAX, Reg EAX), True)

resolveFunctionNumber :: Ident -> Ident -> Latte(Maybe [Ident], Integer)
resolveFunctionNumber classId functionId = do
  store <- get
  let (parent, _ , attributes, _) = (classesEnv store) M.! classId
  let functionAttributes = M.fromList $ filter (\attribute -> case attribute of {
    ((_, FunctionAttribute), _) -> True;
    _ -> False
  }) (M.toList attributes)
  case parent of
    Nothing -> do
      let i = elemIndex functionId (map fst (M.keys functionAttributes))
      case i of
        Nothing -> return (Just $ map fst (M.keys functionAttributes), toInteger $ M.size functionAttributes)
        (Just a) -> return (Nothing, toInteger a)
    (Just parentId) -> do
      (parentAttributes, lenOrIdx) <- resolveFunctionNumber parentId functionId
      case parentAttributes of
        Nothing -> return (Nothing, lenOrIdx)
        (Just used) -> do
          let i = elemIndex functionId ((filterExisting (map fst $ M.keys functionAttributes) used []))
          case i of
            Nothing -> let nused = nub $ used ++ (map fst $ M.keys functionAttributes) in
              return (Just nused, toInteger $ length nused)
            (Just a) -> return (Nothing, (toInteger a) + lenOrIdx)

filterExisting :: [Ident] -> [Ident] -> [Ident] -> [Ident]
filterExisting [] _ accumulator = reverse accumulator
filterExisting (id:rest) existing accumulator =
  if elem id existing then filterExisting rest existing accumulator
  else filterExisting rest existing (id:accumulator)