module Frontend.StaticAnalysis.AnalyserExpression (analyzeExpression) where

import Prelude hiding (id)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import qualified Data.Map as M (member, (!), lookup)

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.Grammar.PosLatte (getPosType, getPosExpr)
import Frontend.StaticAnalysis.Definitions (Checker)
import Frontend.StaticAnalysis.AnalyserExpressionBounds (analyzeExpressionBounds)
import Frontend.StaticAnalysis.AnalyserExpressionAssignability (rightAssignableToLeft)
import Frontend.StaticAnalysis.ErrorMessages (typeMismatch, tooManyArgumentsToFunction, notEnoughArgumentsToFunction,
  classDoesNotExist, attributeIsNotPropertyOfClass, alternativeOnWrongTypes, conjunctionOnWrongTypes,
  incomparableTypes, booleanComparingRules, comparingWrongClassTypes, classComparingRules, comparingWrongArrayTypes,
  arrayComparingRules, addingUnsupportedTypes, stringSubtraction, multiplyingBetweenIncompatibleTypes, negateNonBool,
  changeSignNonInt, cannotCast, getAtOnNonArray, arrayIndexWrongType, propertyFuncNotAvailable, arrayOnlyProperty,
  newOnNonClass, arrayOfNonIntSize, functionNotDeclared, variableNotAFunction, nestedSelf, cannotCastNonClassToClass,
  variableNotDeclared, propertyAttrNotAvailable, functionIsNotPropertyOfClass)

analyzeExpression :: Expr ErrPos -> Checker (Type ErrPos)
analyzeExpression expression = analyzeExpressionImpl expression

analyzeExpressionImpl :: Expr ErrPos -> Checker (Type ErrPos)
analyzeExpressionImpl (EVar errPos id) = do
  (currentState, _, _) <- ask
  if M.member id currentState then return $ fst $ currentState M.! id
  else throwError $ variableNotDeclared errPos id
analyzeExpressionImpl (ELitInt errPos _) = return (TInt errPos)
analyzeExpressionImpl (ELitTrue errPos) = return (TBool errPos)
analyzeExpressionImpl (ELitFalse errPos) = return (TBool errPos)
analyzeExpressionImpl (EApp errPos id arguments) = do
  (_, functionEnv, _) <- ask
  if M.member id functionEnv then do
    case functionEnv M.! id of
      fun@(TFun _ returnType parameters)-> do
        areFunctionParametersTypesAssignableFromArgumentsTypes parameters arguments errPos fun 0
        return returnType
      t -> throwError $ variableNotAFunction errPos id t
  else throwError $ functionNotDeclared errPos id
analyzeExpressionImpl (EString errPos _) = return (TStr errPos)
analyzeExpressionImpl (ENewArr errPos typeOfArray sizeExpression) = do
  sizeType <- analyzeExpression sizeExpression
  _ <- analyzeExpressionBounds sizeExpression
  case sizeType of
    (TInt _) -> return (TTab errPos typeOfArray)
    t        -> throwError $ arrayOfNonIntSize errPos t
analyzeExpressionImpl (ENewClass errPos classType) = do
  case classType of
    (TClass _ id) -> do
      (_, _, classEnv) <- ask
      if M.member id classEnv then return classType
      else throwError $ classDoesNotExist errPos id
    _ -> throwError $ newOnNonClass errPos classType
analyzeExpressionImpl (EAttrFun errPos expr attributeId arguments) = do
  expressionType <- analyzeExpression expr
  case expressionType of
    (TClass _ classId) -> do
      propertyType <- analyzeClassFunction errPos classId attributeId Nothing
      case propertyType of
        fun@(TFun _ returnType parameters) -> do
          areFunctionParametersTypesAssignableFromArgumentsTypes parameters arguments errPos fun 0
          return returnType
        _ -> throwError $ functionIsNotPropertyOfClass errPos attributeId classId
    t -> throwError $ propertyFuncNotAvailable errPos t
analyzeExpressionImpl (EAttrType errPos expr attributeId) = do
  expressionType <- analyzeExpression expr
  case expressionType of
    (TClass _ classId) ->
      if attributeId == (Ident "self") then
        throwError $ nestedSelf errPos
      else do
        propertyType <- analyzeClassAttribute errPos classId attributeId Nothing
        case propertyType of
          (TFun _ _ _) -> throwError $ attributeIsNotPropertyOfClass errPos attributeId classId
          _ -> return propertyType
    (TTab _ _) ->
      if attributeId /= (Ident "length") then
        throwError $ arrayOnlyProperty errPos attributeId
      else return (TInt errPos)
    t -> throwError $ propertyAttrNotAvailable errPos t
analyzeExpressionImpl (EArrAt errPos expr indexExpression) = do
  exprType <- analyzeExpression expr
  indexType <- analyzeExpression indexExpression
  _ <- analyzeExpressionBounds indexExpression
  case (exprType, indexType) of
    (TTab _ arrayType, TInt _) -> return arrayType
    (TTab _ _, t) -> throwError $ arrayIndexWrongType errPos t
    (t, _) -> throwError $ getAtOnNonArray errPos t
analyzeExpressionImpl (ENullCast errPos classId) = do
  (_, _, classEnv) <- ask
  if M.member classId classEnv then
    return (TClass errPos classId)
  else
    throwError $ classDoesNotExist errPos classId
analyzeExpressionImpl (ECast errPos classId expr) = do
  exprType <- analyzeExpression expr
  case exprType of
    (TClass _ exprClassId) -> do
      isAssignable <- rightAssignableToLeft (TClass errPos classId) (TClass errPos exprClassId) errPos
      case isAssignable  of
        True -> return (TClass errPos classId)
        _    -> throwError $ cannotCast errPos classId exprClassId
    t -> throwError $ cannotCastNonClassToClass errPos t
analyzeExpressionImpl (Neg errPos expr) = do
  exprType <- analyzeExpression expr
  case exprType of
    (TInt _) -> return (TInt errPos)
    t -> throwError $ changeSignNonInt errPos t
analyzeExpressionImpl (Not errPos expr) = do
  exprType <- analyzeExpression expr
  case exprType of
    (TBool _) -> return (TBool errPos)
    t -> throwError $ negateNonBool errPos t
analyzeExpressionImpl (EMul errPos expr1 operand expr2) = do
  exprType1 <- analyzeExpression expr1
  exprType2 <- analyzeExpression expr2
  case (exprType1, exprType2) of
    (TInt _, TInt _) -> return (TInt errPos)
    (t1, t2) -> throwError $ multiplyingBetweenIncompatibleTypes errPos operand t1 t2
analyzeExpressionImpl (EAdd errPos expr1 operand expr2) = do
  exprType1 <- analyzeExpression expr1
  exprType2 <- analyzeExpression expr2
  case (exprType1, exprType2) of
    (TInt _, TInt _) -> return (TInt errPos)
    (TStr _, TStr _) ->
      case operand of
        Minus _ -> throwError $ stringSubtraction errPos
        _ -> return (TStr errPos)
    (t1, t2) -> throwError $ addingUnsupportedTypes errPos operand t1 t2
analyzeExpressionImpl (ERel errPos expr1 operator expr2) = do
  exprType1 <- analyzeExpression expr1
  exprType2 <- analyzeExpression expr2
  _ <- analyzeExpressionBounds expr1
  _ <- analyzeExpressionBounds expr2
  case (exprType1, exprType2) of
    (TInt _, TInt _) -> return (TBool errPos)
    (TStr _, TStr _) -> return (TBool errPos)
    (TTab _ arrayType1, TTab _ arrayType2) -> do
      isAssignable1 <- rightAssignableToLeft arrayType1 arrayType2 errPos
      isAssignable2 <- rightAssignableToLeft arrayType2 arrayType1 errPos
      if isAssignable1 || isAssignable2 then
        case operator of
          (EQU _) -> return (TBool errPos)
          (NE _) -> return (TBool errPos)
          _ -> throwError $ arrayComparingRules errPos operator
      else throwError $ comparingWrongArrayTypes errPos arrayType1 arrayType2
    (t1@(TClass _ _), t2@(TClass _ _)) -> do
      isAssignable1 <- rightAssignableToLeft exprType1 exprType2 errPos
      isAssignable2 <- rightAssignableToLeft exprType2 exprType1 errPos
      if isAssignable1 || isAssignable2 then
        case operator of
          (EQU _) -> return (TBool errPos)
          (NE _) -> return (TBool errPos)
          _ -> throwError $ classComparingRules errPos operator
      else throwError $ comparingWrongClassTypes errPos t1 t2
    (TBool _, TBool _) ->
      case operator of
        (EQU _) -> return (TBool errPos)
        (NE _) -> return (TBool errPos)
        _ -> throwError $ booleanComparingRules errPos operator
    (t1, t2) -> throwError $ incomparableTypes errPos t1 t2
analyzeExpressionImpl (EAnd errPos expr1 expr2) = do
  exprType1 <- analyzeExpression expr1
  exprType2 <- analyzeExpression expr2
  case (exprType1, exprType2) of
    (TBool _, TBool _) -> return (TBool errPos)
    (t1, t2) -> throwError $ conjunctionOnWrongTypes errPos t1 t2
analyzeExpressionImpl (EOr errPos expr1 expr2) = do
  exprType1 <- analyzeExpression expr1
  exprType2 <- analyzeExpression expr2
  case (exprType1, exprType2) of
    (TBool _, TBool _) -> return (TBool errPos)
    (t1, t2) -> throwError $ alternativeOnWrongTypes errPos t1 t2


analyzeClassAttribute :: ErrPos -> Ident -> Ident -> Maybe Ident -> Checker(Type ErrPos)
analyzeClassAttribute errPos classId attributeId startedFrom = do
  (_, _, classEnv) <- ask
  case M.lookup classId classEnv of
    (Just (Nothing, _, (attributes, _))) ->
      if M.member attributeId attributes then
        return (attributes M.! attributeId)
      else throwError $ attributeIsNotPropertyOfClass errPos attributeId (resolveParent startedFrom classId)
    (Just (Just parentClassId, _, (attributes, _))) ->
      if M.member attributeId attributes then
        return (attributes M.! attributeId)
      else analyzeClassAttribute errPos parentClassId attributeId (resolveStartedFrom startedFrom classId)
    Nothing -> throwError $ classDoesNotExist errPos classId


resolveStartedFrom :: Maybe Ident -> Ident -> Maybe Ident
resolveStartedFrom Nothing id = Just id
resolveStartedFrom parent _ = parent


resolveParent :: Maybe Ident -> Ident -> Ident
resolveParent Nothing id = id
resolveParent (Just id) _ = id


analyzeClassFunction :: ErrPos -> Ident -> Ident -> Maybe Ident -> Checker(Type ErrPos)
analyzeClassFunction errPos classId attributeId startedFrom = do
  (_, _, classEnv) <- ask
  case M.lookup classId classEnv of
    (Just (Nothing, _, (_, functions))) ->
      if M.member attributeId functions then
        return (functions M.! attributeId)
      else throwError $ functionIsNotPropertyOfClass errPos attributeId (resolveParent startedFrom classId)
    (Just (Just parentClassId, _, (_, functions))) ->
      if M.member attributeId functions then
        return (functions M.! attributeId)
      else analyzeClassFunction errPos parentClassId attributeId (resolveStartedFrom startedFrom classId)
    Nothing -> throwError $ classDoesNotExist errPos classId

areFunctionParametersTypesAssignableFromArgumentsTypes :: [Type ErrPos] -> [Expr ErrPos] -> ErrPos -> Type ErrPos -> Integer -> Checker()
areFunctionParametersTypesAssignableFromArgumentsTypes [] [] _ _ _ = return ()
areFunctionParametersTypesAssignableFromArgumentsTypes (_:rest) [] errPos func argIdx =
  throwError $ notEnoughArgumentsToFunction errPos (argIdx) (argIdx + (fromIntegral $ length rest) + 1) func
areFunctionParametersTypesAssignableFromArgumentsTypes [] (_:rest) errPos func argIdx =
  throwError $ tooManyArgumentsToFunction errPos (argIdx + (fromIntegral $ length rest) + 1) (argIdx) func
areFunctionParametersTypesAssignableFromArgumentsTypes (parameter:restParameters) (argument:arguments) errPos func argIdx = do
  argumentType <- analyzeExpression argument
  isAssignable <- rightAssignableToLeft parameter argumentType errPos
  case isAssignable of
    True -> areFunctionParametersTypesAssignableFromArgumentsTypes restParameters arguments errPos func (argIdx + 1)
    _    -> throwError $ typeMismatch errPos parameter (getPosType parameter) argumentType (getPosExpr argument) func argIdx