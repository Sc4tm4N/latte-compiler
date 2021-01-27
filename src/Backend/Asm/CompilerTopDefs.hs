module Backend.Asm.CompilerTopDefs where

import Prelude hiding (id)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import qualified Data.Map as M
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Backend.Asm.Data
import Backend.Asm.ConstantParts
import Backend.Asm.Utils
import Backend.Asm.CompilerStatement
import Backend.Asm.CompilerExpression
import Backend.CompilerData
import Backend.Utils
import Utils.Common (backendDefaultLibraryFunctions)


gatherClassAttributes :: [ClassBody ErrPos] -> Integer -> MapOfClassAttributes -> (MapOfClassAttributes, Integer)
gatherClassAttributes [] size accumulator = (accumulator, size)
gatherClassAttributes ((TypAttr _ attributeType id):rest) size accumulator =
  gatherClassAttributes rest (size + defaultSize) (M.insert (id, DataAttribute) (ClassType size, attributeType) accumulator)
gatherClassAttributes ((FunAttr _ (FunDef _ returnType id argumentTypes _)):rest) size accumulator =
  gatherClassAttributes rest size (M.insert (id, FunctionAttribute) (ClassFunction argumentTypes, returnType) accumulator)

gatherClasses :: [TopDef ErrPos] -> ClassEnv
gatherClasses topDefs =
  let classDefinitions = _gatherClasses topDefs M.empty in
    adjustClassesWithParentDetails (M.toList classDefinitions) classDefinitions

_gatherClasses :: [TopDef ErrPos] -> ClassEnv -> ClassEnv
_gatherClasses [] accumulator = accumulator
_gatherClasses ((ClassDef _ classId ic):rest) accumulator =
  let (attributes, sizeOfClass) = gatherClassAttributes ic defaultSize M.empty in
  _gatherClasses rest (M.insert classId (Nothing, sizeOfClass, attributes, True) accumulator)
_gatherClasses ((ExtendsClassDef _ classId parentClassId ic):rest) accumulator =
  let (attributes, sizeOfClass) = gatherClassAttributes ic defaultSize M.empty in
  _gatherClasses rest (M.insert classId (Just parentClassId, sizeOfClass, attributes, False) accumulator)
_gatherClasses (_:rest) accumulator = _gatherClasses rest accumulator

adjustClassesWithParentDetails :: [(Ident, (Maybe Ident, Integer, MapOfClassAttributes, Bool))] -> ClassEnv -> ClassEnv
adjustClassesWithParentDetails [] accumulator = accumulator
adjustClassesWithParentDetails ((id, (_, _, _, _)):rest) accumulator = case accumulator M.! id of
  (_, _, _, True) -> adjustClassesWithParentDetails rest accumulator
  (_, _, _, False) -> adjustClassesWithParentDetails rest (adjustWithParentsAttributesAndSizes id accumulator)

evaluateSize :: MapOfClassAttributes -> Integer
evaluateSize attributes = foldl (
  \accumulator expression -> case expression of
    (_, (ClassType _, _)) -> accumulator + 4
    _ -> accumulator
  ) 0 (M.toList attributes)

addOffset :: Integer -> [((Ident, AttributeType), (ClassAttribute, Type ErrPos))] -> [((Ident, AttributeType), (ClassAttribute, Type ErrPos))]
addOffset offset rest = map (
  \attribute -> case attribute of
    ((id, at), (ClassType size, attributeType)) -> ((id, at), (ClassType (size + offset), attributeType))
    _ -> attribute
  ) rest

adjustWithParentsAttributesAndSizes :: Ident -> ClassEnv -> ClassEnv
adjustWithParentsAttributesAndSizes id accumulator = case accumulator M.! id of
  (_, _, _, True) -> accumulator
  (Nothing, size, attributes, _) -> M.insert id (Nothing, size, attributes, True) accumulator
  (Just parentId, size, attrs, _) ->
    let parentAccumulator = adjustWithParentsAttributesAndSizes parentId accumulator in
      let (_, _, parentAttributes, _) = parentAccumulator M.! parentId in
        let parentSize = (evaluateSize parentAttributes) in
          M.insert id (
            Just parentId,
            parentSize + size,
            M.fromList (
              (filter
                (\a -> case a of {(_, (ClassType _, _)) -> True; _ -> False })
                (M.toList parentAttributes)
              ) ++ (
                addOffset parentSize (M.toList attrs))), True
          ) parentAccumulator

getFunctionIdForClass :: Ident -> Ident -> String
getFunctionIdForClass classId functionId = "." ++ (show classId) ++ "." ++ (show functionId)

gatherClassFunctions :: [(Ident, (Maybe Ident, Integer, MapOfClassAttributes, Bool))] -> FunEnv -> FunEnv
gatherClassFunctions [] accumulator = accumulator
gatherClassFunctions (((Ident id), (_, _, attributes, _)):rest) accumulator =
  gatherClassFunctions rest (
    foldr (
      \((attributeId, _), (classAttribute, attributeType)) insideAccumulator -> case classAttribute of
        (ClassFunction argumentTypes) -> M.insert (Ident $ getFunctionIdForClass (Ident id) attributeId) ((Arg Nothing (TInt Nothing) (Ident ("class+" ++ id))):argumentTypes, attributeType) insideAccumulator
        _ -> insideAccumulator
    ) accumulator (M.toList attributes)
  )

_gatherFunctions :: [TopDef ErrPos] -> FunEnv -> FunEnv
_gatherFunctions [] accumulator = accumulator
_gatherFunctions ((FnDef _ (FunDef _ returnType id argumentTypes _)):rest) accumulator =
  _gatherFunctions rest (M.insert id (argumentTypes, returnType) accumulator)
_gatherFunctions (_:rest) accumulator = _gatherFunctions rest accumulator

gatherFunctions :: [TopDef ErrPos] -> ClassEnv -> FunEnv
gatherFunctions topDefs classEnv =
  let defaultFunctions = M.fromList backendDefaultLibraryFunctions in
    let definedFunctions = _gatherFunctions topDefs defaultFunctions in
      gatherClassFunctions (M.toList classEnv) definedFunctions

_compile :: [TopDef ErrPos] -> Latte([Instruction])
_compile topDefs = do
  store <- get
  tablesStringList <- addClassesFunctionsCalls (M.keys $ classesEnv store) []
  put $ store { generated = ((reverse tablesStringList) ++ (generated store)) }
  mapM_ generateClass topDefs
  compileFunctions (extractFunctionsFromTopDefs topDefs) ["main"] ["main"]
  store' <- get
  return $ generated store'

generateClass :: TopDef ErrPos -> Latte()
generateClass (ClassDef pos id ics) = do
  store <- get
  put $ store {
    nextVariable = -4,
    variableEnv = M.empty,
    generated = [COMMENT "moving class attributes to local space"]
  }
  loadContext id
  store' <- get -- store' zawiera kontekst, store jest jaki byl przed wejsciem
  let contextUpdate = [POP (Register EBX), POP (Register ECX), POP (Register EDX), POP (Register EAX)] ++ (generated store') ++ [PUSH (Register EAX), PUSH (Register EDX), PUSH (Register ECX), PUSH (Register EBX)]
  put $ store' {
    generated = [COMMENT "updating class attributes with local space changes", COMMENT "UPDATE MARKER - BEG"]
  }
  putContext id
  store'' <- get
  put $ store'' {
    variableEnv = M.insert (Ident "self") (2 * defaultSize, TClass pos id) (variableEnv store''),
    generated = generated store
  }
  mapM_ (generateClassFunction id (generated store') (generated store'') contextUpdate) ics
  store''' <- get
  put $ store''' {
    variableEnv = (variableEnv store)
  }
  return ()
generateClass (ExtendsClassDef pos id _ ics) = generateClass (ClassDef pos id ics)
generateClass _ = return()

generateClassFunction :: Ident -> [Instruction] -> [Instruction] -> [Instruction] -> ClassBody ErrPos -> Latte()
generateClassFunction clsId context suffix contextUpdate (FunAttr _ (FunDef pos t id args b)) = do
  let funId = getFunctionIdForClass clsId id
  store <- get
  put $ store {
    generated = functionPrefix ++ (LABEL funId):(generated store)
  }
  generateFunction (FunDef pos t (Ident funId) args b) context suffix contextUpdate True
  store' <- get
  put $ store' {
    nextVariable = nextVariable store,
    variableEnv = variableEnv store
  }
generateClassFunction _ _ _ _ _ = return ()

loadContext :: Ident -> Latte()
loadContext id = do
  store <- get
  let (_, _, attrs, _) = (classesEnv store) M.! id
  let sortedAttrs = sortBy cmpAttributeEntry (M.toList attrs)
  putAttrs sortedAttrs

cmpAttributeEntry :: ((Ident, AttributeType), (ClassAttribute, Type ErrPos)) -> ((Ident, AttributeType), (ClassAttribute, Type ErrPos)) -> Ordering
cmpAttributeEntry ((Ident id1, _), (ClassFunction _, _)) ((Ident id2, _), (ClassFunction _, _)) = compare id1 id2
cmpAttributeEntry (_, (ClassFunction _, _)) (_, (ClassType _, _)) = LT
cmpAttributeEntry (_, (ClassType _, _)) (_, (ClassFunction _, _))  = GT
cmpAttributeEntry (_, (ClassType i1, _)) (_, (ClassType i2, _)) = compare i1 i2

putContext :: Ident -> Latte()
putContext id = do
  store <- get
  let (_, _, attrs, _) = (classesEnv store) M.! id
  restoreAttrsWithSavedResult (M.toList attrs)

restoreAttrsWithSavedResult :: [((Ident, AttributeType), (ClassAttribute, Type ErrPos))] -> Latte()
restoreAttrsWithSavedResult attributes = do
  store <- get
  put $ store {
    generated = (PUSH (Register EAX)):(COMMENT "saving result before context update"):(generated store)
  }
  restoreAttrs attributes
  store' <- get
  put $ store' {
    generated = (COMMENT "UPDATE MARKER - END"):(POP (Register EAX)):(COMMENT "restoring result after context update"):(generated store')
  }


restoreAttrs :: [((Ident, AttributeType), (ClassAttribute, Type ErrPos))] -> Latte()
restoreAttrs [] = return ()
restoreAttrs (((Ident id, _), (ClassType i, _)):attrs) = do
  store <- get
  ((var, _, _), _) <- generateExpression (EVar Nothing (Ident $ "self+" ++ id)) [] []
  movl1 <- printMov var (Reg EDX) False
  movl2 <- printMov (Stack $ 2 * defaultSize) (Reg EAX) False
  put $ store {
    generated = (MOV (Register EDX) (MemoryCall (RegisterMemory EAX JustCall))):(ADD (Const i) (Register EAX)):movl2
      ++ movl1
      ++ (COMMENT ("updating class attribute " ++ id)):(generated store) }
  restoreAttrs attrs
restoreAttrs ((_, _):attrs) = restoreAttrs attrs

putAttrs :: [((Ident, AttributeType), (ClassAttribute, Type ErrPos))] -> Latte()
putAttrs [] = return ()
putAttrs (((Ident id, _), (ClassType i, t)):attrs) = do
  store <- get
  movl1 <- printMov (Stack $ 2 * defaultSize) (Reg EAX) False
  movl2 <- printMov (Reg EAX) (Stack (nextVariable store)) False
  put $ store {
    variableEnv = M.insert (Ident $ "self+" ++ id) (nextVariable store, t) (variableEnv store),
    nextVariable = (nextVariable store) - defaultSize,
    generated =
        movl2
        ++ [(MOV (MemoryCall (RegisterMemory EAX JustCall)) (Register EAX)), (ADD (Const i) (Register EAX))]
        ++ movl1
        ++ (COMMENT ("moving to local scope class attribute: " ++ id)):(generated store)
    }
  store' <- get
  putAttrs attrs
putAttrs ((_, _):attrs) = putAttrs attrs

compileFunctions :: M.Map String (FunDef ErrPos) -> [String] -> [String] -> Latte()
compileFunctions allFunctionDefinitions (functionId:rest) usedIds = do
  if M.member functionId allFunctionDefinitions then do
    store <- get
    put $ store {
      variableEnv = M.empty,
      nextVariable = -4,
      generated = functionPrefix ++ (LABEL functionId):(generated store) }
    generateFunction (allFunctionDefinitions M.! functionId) [] [] [] True
    store' <- get
    compileFunctions allFunctionDefinitions (((nub (usedFunctions store')) \\ usedIds) ++ rest) (nub $ (usedFunctions store') ++ usedIds)
  else compileFunctions allFunctionDefinitions rest usedIds
compileFunctions allFunctionDefinitions [] usedIds = do
  store <- get
  put $ store { generated = (COMMENT "functions not used in this module (but compiled and linkable)"):(generated store) }
  compileUnusedFunctions (M.toList allFunctionDefinitions) usedIds

countEspOffset :: Ident -> [Stmt ErrPos] -> Latte(Integer)
countEspOffset functionId statements = do
  store <- get
  let (arguments, _) = (functionsEnv store) M.! functionId
  return $ ((((defaultSize * ((toInteger $ length arguments) + (countLocalVariables statements)) - ((nextVariable store) + defaultSize)) `div` 16) + 1) * 16)

movArgs :: Ident -> Latte()
movArgs functionId = do
  store <- get
  put $ store { generated = (COMMENT "moving function arguments to local space"):(generated store) }
  _movArgs (fst $ (functionsEnv store) M.! functionId) 0

_movArgs :: [Arg ErrPos] -> Integer -> Latte()
_movArgs [] _ = return ()
_movArgs ((Arg _ argType argId):args) accumulator = do
  store <- get
  put $ store {
    variableEnv = (M.insert argId (nextVariable store, argType) (variableEnv store)),
    nextVariable = (nextVariable store) - defaultSize,
    generated = (movArgInstructions accumulator (nextVariable store)) ++ (COMMENT ("moving to local scope function argument: " ++ show argId)):(generated store) }
  store' <- get
  _movArgs args (accumulator + defaultSize)

movArgInstructions :: Integer -> Integer -> [Instruction]
movArgInstructions i j = [
  MOV (Register EAX) (MemoryCall (RegisterMemory EBP (Addiction (Const j)))),
  MOV (MemoryCall (RegisterMemory EBP (Addiction (Const (8 + i))))) (Register EAX)]


generateFunction :: FunDef ErrPos -> [Instruction] -> [Instruction] -> [Instruction] -> Bool -> Latte()
generateFunction (FunDef _ _ functionId _ (Block _ statements)) context suffix contextUpdate _ = do
  espSub <- countEspOffset functionId statements
  store <- get
  put $ store { generated = context ++ ((SUB (Const espSub) (Register ESP)):(COMMENT "reserve space for local variables while keeping stack aligned"):(generated store)) }
  movArgs functionId
  _ <- genStatements statements suffix contextUpdate
  store' <- get
  put $ store' {
    generated = functionSuffix ++ (suffix ++ (generated store')),
    variableEnv = (variableEnv store)
  }
  return ()

compileUnusedFunctions :: [(String, FunDef ErrPos)] -> [String] -> Latte()
compileUnusedFunctions [] _ = return ()
compileUnusedFunctions ((functionId, functionDef):rest) usedIds = do
  if elem functionId usedIds then
    compileUnusedFunctions rest usedIds
  else do
    store <- get
    put $ store {
      nextVariable = -4,
      generated = functionPrefix ++ (LABEL functionId):(generated store)
    }
    generateFunction functionDef [] [] [] False
    compileUnusedFunctions rest usedIds

extractFunctionsFromTopDefs :: [TopDef ErrPos] -> M.Map String (FunDef ErrPos)
extractFunctionsFromTopDefs topDefs = M.fromList $ map
  (\(FnDef _ (fd@(FunDef _ _ (Ident i) _ _))) -> (i, fd))
  (filter
    (\topDef -> case topDef of
      (FnDef _ _) -> True
      _ -> False
    )
    topDefs)

addClassesFunctionsCalls :: [Ident] -> [Instruction] -> Latte([Instruction])
addClassesFunctionsCalls [] accumulator = return accumulator
addClassesFunctionsCalls (classId:rest) accumulator = do
  parents <- getParentsList classId []
  callTableString <- addClassFunctionsCalls parents [] ""
  addClassesFunctionsCalls rest ((BARE ("." ++ (show classId) ++ "Functions: .int " ++ callTableString)):accumulator)

addClassFunctionsCalls :: [Ident] -> [Ident] -> String -> Latte(String)
addClassFunctionsCalls [] _ accumulator = return accumulator
addClassFunctionsCalls (classId:parents) processedAttributes accumulator = do
  store <- get
  let (_, _ , attributes, _) = (classesEnv store) M.! classId
  functionCalls <- foldM (
    \inAccumulator (attrKey, attr) -> case attr of
      (ClassFunction _, _) -> do
        let (attrId, attrType) = attrKey
        if elem attrId processedAttributes then
          return inAccumulator
        else do
          firstContainingDescendant <- getFirstContaining (reverse $ classId:parents) attrId
          if inAccumulator == "" then
            return (getFunctionIdForClass firstContainingDescendant attrId)
          else
            return (inAccumulator ++ ", " ++ (getFunctionIdForClass firstContainingDescendant attrId))
      _ -> return inAccumulator
    ) accumulator (M.toList attributes)
  let newProcessedAttributes = nub $ (map fst $ M.keys attributes) ++ processedAttributes
  addClassFunctionsCalls parents newProcessedAttributes functionCalls

getParentsList :: Ident -> [Ident] -> Latte([Ident])
getParentsList classId accumulator = do
  store <- get
  let (parent, _ , _, _) = (classesEnv store) M.! classId
  case parent of
    Nothing -> return (classId:accumulator)
    (Just parentId) -> getParentsList parentId (classId:accumulator)

getFirstContaining :: [Ident] -> Ident -> Latte(Ident)
getFirstContaining [] _ = throwError $ impossiblePattern
getFirstContaining (classId:parents) attributeId = do
  store <- get
  let (_, _ , attributes, _) = (classesEnv store) M.! classId
  case (M.lookup (attributeId, FunctionAttribute) attributes) of
    (Just _) -> return classId
    Nothing -> getFirstContaining parents attributeId