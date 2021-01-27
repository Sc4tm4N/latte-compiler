module Frontend.StaticAnalysis.Analyser (staticallyAnalyze) where

import Prelude hiding (Int, lookup, id, all)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List as L (intersect)
import qualified Data.Map as M (Map, keys, member, lookup, fromList, toList, unionWith, insert, (!), empty, map)

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Frontend.Grammar.PosLatte
import Utils.Common
import Frontend.StaticAnalysis.Definitions (CheckerState, CheckerClassEnv, ClassAttributes, Checker,
  CheckerFunctionEnv, CheckerFunctionEnv)
import Frontend.StaticAnalysis.AnalyserExpressionLValue (analyzeExpressionLValue)
import Frontend.StaticAnalysis.AnalyserExpressionConstantsEvaluator (evaluateConstantExpressionToBool)
import Frontend.StaticAnalysis.AnalyserExpression (analyzeExpression)
import Frontend.StaticAnalysis.AnalyserExpressionBounds (analyzeExpressionBounds)
import Frontend.StaticAnalysis.AnalyserExpressionEquality (areSameTypes)
import Frontend.StaticAnalysis.AnalyserExpressionAssignability (rightAssignableToLeft, canAssign)
import Frontend.StaticAnalysis.AnalyserRestrictedNames (isRestricted)
import Frontend.StaticAnalysis.AnalyserError
import Frontend.StaticAnalysis.ErrorMessages (duplicateFunctionIds, duplicateClassIds, voidFunctionParameter,
   duplicatedFunctionParameterIds, duplicatedDeclarations, declarationOfVoidVariable, decrementingNonInt,
   incrementingNonInt, returnTypeMismatch, voidReturnOfNonVoidFunction, conditionWrongType, iterationOverNonTab,
   mainNotDeclared, mainWrongSignature, returnNotAccessible, variableInitTypeMismatch, assignTypeMismatch,
   voidReturnNotAllowed, forTypeMismatch, impossiblePattern, classAttributeAlreadyDefined, circularDependency,
   parentDoesNotExist, classTriesToRedefineArgumentFromSuperclass, classTriesToRedefineFunctionFromSuperclass,
   identifierRestrictedToLanguage, selfNotAssignable, variableInitTypeNotAssignable, assignTypeNotAssignable,
   forTypeNotAssignable)


staticallyAnalyze :: Program ErrPos -> IO (Maybe String)
staticallyAnalyze (Program _ topDefs) = do
  case (gatherFunctionsWithDuplicateCheck topDefs (M.fromList defaultLibraryFunctions),
      gatherClassesWithDuplicateCheck topDefs M.empty) of
    (Left functionDeclarations, Left classDeclarations) ->
      case M.lookup (Ident "main") functionDeclarations of
        Nothing -> return $ Just mainNotDeclared
        (Just (TFun _ (TInt _) [])) -> do
          res <- runExceptT (runReaderT (staticallyAnalyzeTopDefs topDefs) (M.empty, functionDeclarations, classDeclarations))
          case res of
            Left s -> return $ Just s
            Right _ -> return Nothing
        Just (TFun errPos _ _) -> return $ Just $ mainWrongSignature errPos
        Just _ -> return $ Just $ mainWrongSignature Nothing
    (_, Right errMsg) -> return $ Just errMsg
    (Right errMsg, _) -> return $ Just errMsg


------------------------------------------------------------------------------------------------------------------------


gatherFunctionsWithDuplicateCheck :: [TopDef ErrPos] -> CheckerFunctionEnv -> Either CheckerFunctionEnv String
gatherFunctionsWithDuplicateCheck [] env = Left env
gatherFunctionsWithDuplicateCheck ((FnDef errPos (FunDef _ retType id params _)):fs) env =
  if M.member id env then
    Right $ duplicateFunctionIds errPos id (getPosType (env M.! id))
  else if isRestricted id then
    Right $ identifierRestrictedToLanguage errPos id
  else case staticallyAnalyzeFunctionHeader params [] of
    Nothing     -> gatherFunctionsWithDuplicateCheck fs (M.insert id (TFun errPos retType (extractTypesFromArgs params)) env)
    Just errMsg -> Right errMsg
gatherFunctionsWithDuplicateCheck (_:fs) env = gatherFunctionsWithDuplicateCheck fs env



staticallyAnalyzeFunctionHeader :: [Arg ErrPos] -> [Ident] -> Maybe String
staticallyAnalyzeFunctionHeader [] _ = Nothing
staticallyAnalyzeFunctionHeader ((Arg errPos argType id):args) ids =
  if argType == (TVoid Nothing) then Just $ voidFunctionParameter errPos
  else if (elem id ids) then Just $ duplicatedFunctionParameterIds errPos
  else if isRestricted id then Just $ identifierRestrictedToLanguage errPos id
  else (staticallyAnalyzeFunctionHeader args (id:ids))


------------------------------------------------------------------------------------------------------------------------

validateClasses :: CheckerClassEnv -> Either CheckerClassEnv String
validateClasses env =
  case (isClassesInheritanceTree env (M.toList env) Nothing []) of
    Nothing ->
      case (classesDoNotRedefineAttributes env (M.toList env) Nothing [] M.empty) of
        Nothing ->
          case (subClassesDoNotChangeSignature env (M.toList env) Nothing M.empty) of
            Nothing -> Left env
            Just errMsg -> Right errMsg
        Just errMsg -> Right errMsg
    Just errMsg -> Right errMsg

subClassesDoNotChangeSignature :: CheckerClassEnv -> [(Ident, (Maybe Ident, ErrPos, ClassAttributes))] -> Maybe Ident -> M.Map Ident (Type ErrPos) -> Maybe String
subClassesDoNotChangeSignature _ [] Nothing _ = Nothing
subClassesDoNotChangeSignature classEnv ((id, _):rest) Nothing _ =
  subClassesDoNotChangeSignature classEnv rest (Just id) M.empty
subClassesDoNotChangeSignature classEnv rest (Just id) definedFunctions =
  case M.lookup id classEnv of
    Just (Just parentClassId, _, (_, functions)) ->
      let commonAttributesIdentifiers = L.intersect (M.keys functions) (M.keys definedFunctions) in
        if (length commonAttributesIdentifiers) > 0 then do
          case compareClassFunctionsSignatures classEnv commonAttributesIdentifiers definedFunctions functions of
            Nothing -> Nothing
            Just errMsg -> Just errMsg
        else
          subClassesDoNotChangeSignature classEnv rest (Just parentClassId) (M.unionWith (\a _ -> a) functions definedFunctions)
    Just (Nothing, _, (_, functions)) ->
      let commonAttributesIdentifiers = L.intersect (M.keys functions) (M.keys definedFunctions) in
        if (length commonAttributesIdentifiers) > 0 then
          case compareClassFunctionsSignatures classEnv commonAttributesIdentifiers definedFunctions functions of
            Nothing -> Nothing
            Just errMsg -> Just errMsg
        else
          subClassesDoNotChangeSignature classEnv rest Nothing M.empty
    _ -> Just $ impossiblePattern

compareClassFunctionsSignatures :: CheckerClassEnv -> [Ident] -> M.Map Ident (Type ErrPos) -> M.Map Ident (Type ErrPos) -> Maybe String
compareClassFunctionsSignatures _ [] _ _ = Nothing
compareClassFunctionsSignatures env (id:rest) definedFunctions newFunctions = do
  case canAssign env (definedFunctions M.! id) (newFunctions M.! id) of
    True -> compareClassFunctionsSignatures env rest definedFunctions newFunctions
    False -> Just $ classTriesToRedefineFunctionFromSuperclass (newFunctions M.! id) (definedFunctions M.! id)

classesDoNotRedefineAttributes :: CheckerClassEnv -> [(Ident, (Maybe Ident, ErrPos, ClassAttributes))] -> Maybe Ident -> [Ident] -> M.Map Ident (Type ErrPos) -> Maybe String
classesDoNotRedefineAttributes _ [] Nothing _ _ = Nothing
classesDoNotRedefineAttributes classEnv ((id, _):rest) Nothing _ _ =
  classesDoNotRedefineAttributes classEnv rest (Just id) [] M.empty
classesDoNotRedefineAttributes classEnv rest (Just id) definedAttributesIds attributesDefiningMap =
  case M.lookup id classEnv of
    Just (Just parentClassId, _, (attributes, _)) ->
      let commonAttributesIdentifiers = L.intersect (M.keys attributes) definedAttributesIds in
        if (length commonAttributesIdentifiers) > 0 then
          Just $ classTriesToRedefineArgumentFromSuperclass $ redefinedPositions attributes attributesDefiningMap commonAttributesIdentifiers []
        else
          classesDoNotRedefineAttributes classEnv rest (Just parentClassId) (definedAttributesIds ++ M.keys attributes) (M.unionWith (\a _ -> a) attributesDefiningMap attributes)
    Just (Nothing, _, (attributes, _)) ->
      let commonAttributesIdentifiers = L.intersect (M.keys attributes) definedAttributesIds in
        if (length commonAttributesIdentifiers) > 0 then
          Just $ classTriesToRedefineArgumentFromSuperclass $ redefinedPositions attributes attributesDefiningMap commonAttributesIdentifiers []
        else
          classesDoNotRedefineAttributes classEnv rest Nothing [] M.empty
    _ -> Just $ impossiblePattern

redefinedPositions :: M.Map Ident (Type ErrPos) -> M.Map Ident (Type ErrPos) -> [Ident] -> [(ErrPos, ErrPos)] -> [(ErrPos, ErrPos)]
redefinedPositions _ _ [] acc = acc
redefinedPositions definedAttributes attributes (id:rest) acc =
  let pos1 = getPosType (definedAttributes M.! id) in
    let pos2 = getPosType (attributes M.! id) in
      redefinedPositions definedAttributes attributes rest ((pos1, pos2):acc)

isClassesInheritanceTree :: CheckerClassEnv -> [(Ident, (Maybe Ident, ErrPos, ClassAttributes))] -> Maybe Ident -> [Ident] -> Maybe String
isClassesInheritanceTree _ [] Nothing _ = Nothing
isClassesInheritanceTree classEnv ((id, _):rest) Nothing _ =
  isClassesInheritanceTree classEnv rest (Just id) [id]
isClassesInheritanceTree classEnv rest (Just id) visited =
  case M.lookup id classEnv of
    Just (Just parentClassId, classDeclErrPos, _) ->
      if elem parentClassId visited then
        case M.lookup parentClassId classEnv of
          Just (_, childClassDeclErrPos, _) -> Just $ circularDependency classDeclErrPos id childClassDeclErrPos
          _ -> Just $ impossiblePattern
      else
        if M.member parentClassId classEnv then
          isClassesInheritanceTree classEnv rest (Just parentClassId) ((parentClassId):visited)
        else
          Just $ parentDoesNotExist classDeclErrPos parentClassId
    Just (Nothing, _, _) ->
      isClassesInheritanceTree classEnv rest Nothing []
    _ -> Just $ impossiblePattern



gatherClassesWithDuplicateCheck :: [TopDef ErrPos] -> CheckerClassEnv -> Either CheckerClassEnv String
gatherClassesWithDuplicateCheck [] env =
  case validateClasses env of
    Left validated -> Left validated
    Right errMsg -> Right errMsg
gatherClassesWithDuplicateCheck ((ClassDef errPos id body):fs) env =
  if M.member id env then
    let (_, pErrPos, _) = env M.! id in
      Right $ duplicateClassIds errPos id pErrPos
  else if isRestricted id then
    Right $ identifierRestrictedToLanguage errPos id
  else
    case createClassAttributes body (M.empty, M.empty) of
      Left ok -> gatherClassesWithDuplicateCheck fs (M.insert id (Nothing, errPos, ok) env)
      Right errMsg -> Right errMsg
gatherClassesWithDuplicateCheck ((ExtendsClassDef errPos id id2 body):fs) env =
  if M.member id env then
    let (_, pErrPos, _) = env M.! id in
      Right $ duplicateClassIds errPos id pErrPos
  else if isRestricted id then
    Right $ identifierRestrictedToLanguage errPos id
  else
    case createClassAttributes body (M.empty, M.empty) of
      Left ok -> gatherClassesWithDuplicateCheck fs (M.insert id (Just id2, errPos, ok) env)
      Right errMsg -> Right errMsg
gatherClassesWithDuplicateCheck (_:fs) env = gatherClassesWithDuplicateCheck fs env


createClassAttributes :: [ClassBody ErrPos] -> ClassAttributes -> Either ClassAttributes String
createClassAttributes [] classAttributes = Left classAttributes
createClassAttributes ((FunAttr errPos (FunDef _ retType id paramTypes _)):cs) (attributes, functions) =
  if M.member id functions then
    Right $ classAttributeAlreadyDefined errPos id
  else if isRestricted id then
    Right $ identifierRestrictedToLanguage errPos id
  else createClassAttributes cs (attributes, M.insert id (TFun errPos retType (extractTypesFromArgs paramTypes)) functions)
createClassAttributes ((TypAttr errPos aType id):cs) (attributes, functions) =
  if M.member id attributes then
    Right $ classAttributeAlreadyDefined errPos id
  else if isRestricted id then
    Right $ identifierRestrictedToLanguage errPos id
  else createClassAttributes cs (M.insert id aType attributes, functions)

------------------------------------------------------------------------------------------------------------------------

staticallyAnalyzeTopDefs :: [TopDef ErrPos] -> Checker()
staticallyAnalyzeTopDefs [] = return ()
staticallyAnalyzeTopDefs all = do
  errorReachable <- errorReachableEnv all
  _staticallyAnalyzeTopDefs all errorReachable where
    _staticallyAnalyzeTopDefs [] _ = return ()
    _staticallyAnalyzeTopDefs (t:rest) env = do
      staticallyAnalyzeTopDef t env
      _staticallyAnalyzeTopDefs rest env
--  mapM_ staticallyAnalyzeTopDef td errorReachable


staticallyAnalyzeTopDef :: TopDef ErrPos -> ErrorReachableEnv -> Checker()
staticallyAnalyzeTopDef (FnDef _ fd) env = staticallyAnalyzeFunction fd env
staticallyAnalyzeTopDef (ClassDef errPos id body) env = staticallyAnalyzeClass errPos id body env
staticallyAnalyzeTopDef (ExtendsClassDef errPos id _ body) env = staticallyAnalyzeClass errPos id body env


staticallyAnalyzeFunction :: FunDef ErrPos-> ErrorReachableEnv -> Checker()
staticallyAnalyzeFunction (FunDef errPos retType id params (Block _ statements)) errorEnv =
  local (\(checkerState, functionEnv, checkerClassEnv) ->
    (M.insert (Ident "return") (retType, True) (modifyStateWithArgs params checkerState), functionEnv, checkerClassEnv)) $ do
      case retType of
        TVoid _ -> do -- can return but does not need to
          _ <- staticallyAnalyzeBlock (reverseStatementsAndRegularize statements) False
          return()
        _ -> do -- must return
          returnAccessible <- staticallyAnalyzeBlock (reverseStatementsAndRegularize statements) True
          case returnAccessible of
             False -> do
               (state, _, _) <- ask
               if M.member (Ident "self") state then do
                 let (TClass _ classId, _) = state M.! (Ident "self")
                 case M.lookup (Just classId, id) errorEnv of
                   (Just SurelyNo) -> throwError $ returnNotAccessible errPos id
                   (Nothing) -> throwError $ returnNotAccessible errPos id
                   _ -> return()
               else
                 case M.lookup (Nothing, id) errorEnv of
                   (Just SurelyNo) -> throwError $ returnNotAccessible errPos id
                   (Nothing) -> throwError $ returnNotAccessible errPos id
                   _ -> return()
             _     -> return()

------------------------------------------------------------------------------------------------------------------------


modifyStateWithArgs :: [Arg ErrPos] -> CheckerState -> CheckerState
modifyStateWithArgs [] state = state
modifyStateWithArgs ((Arg _ aType aId):args) state = modifyStateWithArgs args (M.insert aId (aType, True) state)


staticallyAnalyzeBlock :: [Stmt ErrPos] -> Bool -> Checker(Bool)
staticallyAnalyzeBlock stmts _ =
 local (\(checkerState, fFunctionEnv, cClassEnv) ->
  (makeAllInStateNotCurrentBlock checkerState, fFunctionEnv, cClassEnv)) $ _internalImplementation stmts False where
    _internalImplementation [] returnAccessible = return (returnAccessible)
    _internalImplementation (s:restStatements) returnAccessible = do
      statementAnalysisResult <- staticallyAnalyzeStatement s
      case statementAnalysisResult of
        (Just declarations, _) -> do
          (currentState, _, _) <- ask
          newState <- tryUpdateStateWithDeclarations declarations currentState $ show s
          local (\(_, functionEnv, classEnv) -> (newState, functionEnv, classEnv)) $ _internalImplementation restStatements returnAccessible
        (Nothing, returnAccessibleFromStatement) -> do
          _internalImplementation restStatements (returnAccessibleFromStatement || returnAccessible)


tryUpdateStateWithDeclarations :: [(Ident, Type ErrPos)] -> CheckerState -> String -> Checker(CheckerState)
tryUpdateStateWithDeclarations ((id, t):ds) currentState err =
  case M.lookup id currentState of
    Just (_, True) -> throwError $ duplicatedDeclarations (getPosType t) id
    _ -> tryUpdateStateWithDeclarations ds (M.insert id (t, True) currentState) err
tryUpdateStateWithDeclarations [] state _ = return state


makeAllInStateNotCurrentBlock :: CheckerState -> CheckerState
makeAllInStateNotCurrentBlock state = M.fromList (map makeNotCurrentBlock (M.toList state)) where
    makeNotCurrentBlock (id, (aType, _)) = (id, (aType, False))


-- declarations, return accessible
staticallyAnalyzeStatement :: Stmt ErrPos -> Checker (Maybe [(Ident, Type ErrPos)], Bool)
staticallyAnalyzeStatement (Empty _) = return (Nothing, False)
staticallyAnalyzeStatement (BStmt _ (Block _ statements)) = do
  returnAccessibleFromStatement <- staticallyAnalyzeBlock statements False
  return (Nothing, returnAccessibleFromStatement)
staticallyAnalyzeStatement stmt@(Decl _ _ _) =
  _validateDeclaration stmt [] where
    _validateDeclaration (Decl errPos declarationType (item:restItems)) done =
      case declarationType of
        (TVoid _) -> throwError $ declarationOfVoidVariable errPos
        (TTab _ _) ->
          case item of
            Init initErrPos itemId expression ->
              if isRestricted itemId then
                throwError $ identifierRestrictedToLanguage initErrPos itemId
              else do
                expressionType <- analyzeExpression expression
                _ <- analyzeExpressionBounds expression
                sameType <- areSameTypes declarationType expressionType initErrPos
                case sameType of
                  True -> _validateDeclaration (Decl initErrPos declarationType restItems) ((itemId, declarationType):done)
                  _ -> throwError $ variableInitTypeMismatch initErrPos declarationType expressionType
            NoInit noInitErrPos itemId ->
              if isRestricted itemId then
                throwError $ identifierRestrictedToLanguage noInitErrPos itemId
              else do
                _validateDeclaration (Decl errPos declarationType restItems) ((itemId, declarationType):done)
        _ ->
          case item of
            Init initErrPos itemId expression ->
              if isRestricted itemId then
                throwError $ identifierRestrictedToLanguage initErrPos itemId
              else do
                expressionType <- analyzeExpression expression
                _ <- analyzeExpressionBounds expression
                isAssignable <- rightAssignableToLeft declarationType expressionType initErrPos
                case isAssignable of
                  True -> _validateDeclaration (Decl initErrPos declarationType restItems) ((itemId, declarationType):done)
                  _ -> throwError $ variableInitTypeNotAssignable initErrPos declarationType expressionType
            NoInit noInitErrPos itemId ->
              if isRestricted itemId then
                throwError $ identifierRestrictedToLanguage noInitErrPos itemId
              else do
                _validateDeclaration (Decl errPos declarationType restItems) ((itemId, declarationType):done)
    _validateDeclaration (Decl _ _ []) done = return $ (Just done, False)
    _validateDeclaration _ _ = throwError $ impossiblePattern
staticallyAnalyzeStatement (Ass errPos expr1 expr2) =
  case expr1 of
    (EVar _ (Ident "self")) -> throwError $ selfNotAssignable errPos
    _ -> do
      expressionType1 <- analyzeExpression expr1
      _ <- analyzeExpressionLValue expr1
      expressionType2 <- analyzeExpression expr2
      _ <- analyzeExpressionBounds expr2
      case (expressionType1, expressionType2) of
        (t1@(TTab _ _), t2@(TTab _ _)) -> do
          sameType <- areSameTypes expressionType1 expressionType2 errPos
          case sameType of
            True -> return (Nothing, False)
            _ -> throwError $ assignTypeMismatch errPos t1 t2
        (t1, t2) -> do
          isAssignable <- rightAssignableToLeft expressionType1 expressionType2 errPos
          case isAssignable of
            True -> return (Nothing, False)
            _ -> throwError $ assignTypeNotAssignable errPos t1 t2
staticallyAnalyzeStatement (Incr errPos expr) = do
  expressionType <- analyzeExpression expr
  _ <- analyzeExpressionBounds expr
  case expressionType of
    (TInt _) -> return (Nothing, False)
    _ -> throwError $ incrementingNonInt errPos
staticallyAnalyzeStatement (Decr errPos expr) = do
  _ <- analyzeExpressionBounds expr
  expressionType <- analyzeExpression expr
  _ <- analyzeExpressionBounds expr
  case expressionType of
    (TInt _) -> return (Nothing, False)
    _ -> throwError $ decrementingNonInt errPos
staticallyAnalyzeStatement (Ret errPos expr) = do
  expressionType <- analyzeExpression expr
  _ <- analyzeExpressionBounds expr
  (currentState, _, _) <- ask
  let retType = getCurrentReturnType currentState
  isAssignable <- rightAssignableToLeft retType expressionType errPos
  case isAssignable of
    True ->
      if isVoid retType then
        throwError $ voidReturnNotAllowed errPos
      else return (Nothing, True)
    _ -> throwError $ returnTypeMismatch errPos expressionType retType
staticallyAnalyzeStatement (VRet errPos) = do
  (currentState, _, _) <- ask
  case getCurrentReturnType currentState of
    (TVoid _) -> return (Nothing, True)
    _ -> throwError $ voidReturnOfNonVoidFunction errPos
staticallyAnalyzeStatement (Cond errPos expression statement) = do
  expressionType <- analyzeExpression expression
  staticValueOfExpression <- evaluateConstantExpressionToBool expression
  case expressionType of
    (TBool _) -> do
      (_, returnAccessible) <- staticallyAnalyzeStatement statement
      case staticValueOfExpression of
        (Just True) -> return (Nothing, returnAccessible)
        _ -> return (Nothing, False)
    t -> throwError $ conditionWrongType errPos t
staticallyAnalyzeStatement (CondElse errPos expression statement1 statement2) = do
  expressionType <- analyzeExpression expression
  staticValueOfExpression <- evaluateConstantExpressionToBool expression
  case expressionType of
    (TBool _) -> do
      (_, returnAccessible1) <- staticallyAnalyzeStatement statement1
      (_, returnAccessible2) <- staticallyAnalyzeStatement statement2
      case staticValueOfExpression of
        Just True  -> return (Nothing, returnAccessible1)
        Just False -> return (Nothing, returnAccessible2)
        _ -> return (Nothing, returnAccessible1 && returnAccessible2)
    t -> throwError $ conditionWrongType errPos t
staticallyAnalyzeStatement (While errPos expression statement) = do
  expressionType <- analyzeExpression expression
  case expressionType of
    (TBool _) -> do
      (_, returnAccessible) <- staticallyAnalyzeStatement statement
      return(Nothing, returnAccessible)
    t -> throwError $ conditionWrongType errPos t
staticallyAnalyzeStatement (For errPos variableType variableId expression stmt) = do
  expressionType <- analyzeExpression expression
  case expressionType of
    (TTab _ arrayType) ->
      case arrayType of
        (TTab _ _) -> do
          sameType <- areSameTypes variableType arrayType errPos
          case sameType of
            True -> do
              (_, returnAccessible) <- staticallyAnalyzeStatement $ insertDeclarationToBlockOrCreateBlock variableType variableId stmt errPos 0
              return(Nothing, returnAccessible)
            _ -> throwError $ forTypeMismatch errPos variableType arrayType
        _ -> do
          isAssignable <- rightAssignableToLeft variableType arrayType errPos
          case isAssignable of
            True -> do
              (_, returnAccessible) <- staticallyAnalyzeStatement $ insertDeclarationToBlockOrCreateBlock variableType variableId stmt errPos 0
              return(Nothing, returnAccessible)
            _ -> throwError $ forTypeNotAssignable errPos variableType arrayType
    t -> throwError $ iterationOverNonTab errPos t
staticallyAnalyzeStatement (SExp _ expression) = do
  _ <- analyzeExpression expression
  _ <- analyzeExpressionBounds expression
  return (Nothing, False)


getCurrentReturnType :: CheckerState -> Type ErrPos
getCurrentReturnType currentState = fst $ currentState M.! (Ident "return")

isVoid :: Type ErrPos -> Bool
isVoid (TVoid _) = True
isVoid _ = False


getClassAttributes :: Ident -> Checker(CheckerState)
getClassAttributes id = do
  (_, _, classEnv) <- ask
  case M.lookup id classEnv of
    Nothing -> return M.empty
    (Just (Nothing, _, (attributes, _))) -> return (mapClassAttributesToState attributes)
    (Just (Just parentClassId, _, (attributes, _))) -> do
      parentClassAttrs <- getClassAttributes parentClassId
      return (M.unionWith (\a _ -> a) parentClassAttrs (mapClassAttributesToState attributes))

getClassFunctions :: Ident -> Checker(CheckerFunctionEnv)
getClassFunctions id = do
  (_, _, classEnv) <- ask
  case M.lookup id classEnv of
    Nothing -> return M.empty
    (Just (Nothing, _, (_, functions))) -> return functions
    (Just (Just parentClassId, _, (_, functions))) -> do
      parentClassAttrs <- getClassFunctions parentClassId
      return (M.unionWith (\a _ -> a) parentClassAttrs functions)

mapClassAttributesToState :: M.Map Ident (Type ErrPos) -> CheckerState
mapClassAttributesToState cla = M.map (\x -> (x, True)) cla

staticallyAnalyzeClass :: ErrPos -> Ident -> [ClassBody ErrPos] -> ErrorReachableEnv -> Checker()
staticallyAnalyzeClass errPos id body env = do
  attributes <- getClassAttributes id
  functions <- getClassFunctions id
  checkClassFunctions (M.insert (Ident "self") (TClass errPos id, True) attributes) functions body env

checkClassFunctions :: CheckerState -> CheckerFunctionEnv -> [ClassBody ErrPos] -> ErrorReachableEnv -> Checker()
checkClassFunctions _ _ [] _ = return ()
checkClassFunctions attributes functions ((FunAttr _ f):restProperties) env = do
  local (\(state, functionEnv, classEnv) -> (M.unionWith (\a _ -> a) attributes state, M.unionWith (\a _ -> a) functionEnv functions, classEnv)) $ staticallyAnalyzeFunction f env
  checkClassFunctions attributes functions restProperties env
checkClassFunctions attributes functions (_:restProperties) env = checkClassFunctions attributes functions restProperties env

------------------------------------------------------------------------------------------------------------------------

extractTypesFromArgs :: [Arg ErrPos] -> [Type ErrPos]
extractTypesFromArgs [] = []
extractTypesFromArgs ((Arg _ argType _):rest) = argType:(extractTypesFromArgs rest)

-- because for is nested this way -> {{[{{[statements]}}]}}, instead of {{[statements]}}
insertDeclarationToBlockOrCreateBlock :: Type ErrPos -> Ident -> Stmt ErrPos -> ErrPos -> Integer -> Stmt ErrPos
insertDeclarationToBlockOrCreateBlock variableType variableId (BStmt errPos1 (Block errPos2 statements)) higherErrPos depth =
  case statements of
    [x] -> case x of
      BStmt _ _ ->
        case depth of
          0 -> insertDeclarationToBlockOrCreateBlock variableType variableId x higherErrPos (depth + 1)
          _ -> (BStmt errPos1 (Block errPos2 ((Decl higherErrPos variableType [NoInit higherErrPos variableId]):statements)))
      _ -> (BStmt errPos1 (Block errPos2 ((Decl higherErrPos variableType [NoInit higherErrPos variableId]):statements)))
    _ -> (BStmt errPos1 (Block errPos2 ((Decl higherErrPos variableType [NoInit higherErrPos variableId]):statements)))
insertDeclarationToBlockOrCreateBlock variableType variableId originalStatement higherErrPos _ =
  BStmt higherErrPos (Block higherErrPos [Decl higherErrPos variableType [NoInit higherErrPos variableId], originalStatement])
