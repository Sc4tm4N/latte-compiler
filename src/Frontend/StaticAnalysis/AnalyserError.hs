module Frontend.StaticAnalysis.AnalyserError (errorReachableEnv, ErrorReachableEnv,
  ErrorReachable(SurelyYes, SurelyNo, Unsure, RecheckRequired)) where

import Prelude hiding (Int, lookup, id)
import Control.Monad.Reader
import Frontend.Grammar.ShowLatte
import qualified Data.Map as M (Map, lookup, insert, empty)

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Utils.Common
import Frontend.StaticAnalysis.Definitions (Checker)
import Frontend.StaticAnalysis.AnalyserExpressionConstantsEvaluator (evaluateConstantExpressionToBool)


type ErrorReachableEnv = (M.Map (Maybe Ident, Ident) (ErrorReachable))
data ErrorReachable = SurelyYes
  | SurelyNo
  | Unsure
  | RecheckRequired
  deriving(Read, Show)


errorReachableEnv :: [TopDef ErrPos] -> Checker (ErrorReachableEnv)
errorReachableEnv topDefs = do
  let defaultEnv = defaultLibraryFunctionsEnv defaultLibraryFunctions M.empty
  errorReachableEnvRes <- errorReachableEnvTopDefs topDefs defaultEnv
  recursiveCheck topDefs topDefs errorReachableEnvRes 50

recursiveCheck :: [TopDef ErrPos] -> [TopDef ErrPos] -> ErrorReachableEnv -> Integer -> Checker(ErrorReachableEnv)
recursiveCheck _ [] env 0 = return (env)
recursiveCheck topDefs [] env counter = recursiveCheck topDefs topDefs env (counter - 1)
recursiveCheck topDefs (td:rest) env counter = do
  newEnv <- errorReachableEnvTopDef td env
  recursiveCheck topDefs rest newEnv counter

errorReachableEnvTopDefs :: [TopDef ErrPos] -> ErrorReachableEnv -> Checker(ErrorReachableEnv)
errorReachableEnvTopDefs [] env = return (env)
errorReachableEnvTopDefs (td:rest) env = do
  newEnv <- errorReachableEnvTopDef td env
  errorReachableEnvTopDefs rest newEnv


errorReachableEnvTopDef :: TopDef ErrPos -> ErrorReachableEnv -> Checker(ErrorReachableEnv)
errorReachableEnvTopDef (FnDef _ fd) env = errorReachableEnvFunctionDef fd env
errorReachableEnvTopDef (ClassDef _ id body) env = errorReachableEnvClassBody body id env
errorReachableEnvTopDef (ExtendsClassDef _ id _ body) env = errorReachableEnvClassBody body id env

errorReachableEnvClassBody :: [ClassBody ErrPos] -> Ident -> ErrorReachableEnv -> Checker(ErrorReachableEnv)
errorReachableEnvClassBody [] _ env = return(env)
errorReachableEnvClassBody ((FunAttr _ fd):rest) id env = do
  env' <- errorReachableEnvFunctionDefForClass fd id env
  errorReachableEnvClassBody rest id env'
errorReachableEnvClassBody (a:rest) id env = errorReachableEnvClassBody rest id env

errorReachableEnvFunctionDefForClass :: FunDef ErrPos -> Ident -> ErrorReachableEnv -> Checker(ErrorReachableEnv)
errorReachableEnvFunctionDefForClass (FunDef _ _ id _ (Block _ statements)) classId env = do
  res <- errorReachableEnvBlock statements env
  let newEnv = M.insert (Just classId, id) res env
  return(newEnv)

errorReachableEnvFunctionDef :: FunDef ErrPos -> ErrorReachableEnv -> Checker(ErrorReachableEnv)
errorReachableEnvFunctionDef (FunDef _ _ id _ (Block _ statements)) env = do
  res <- errorReachableEnvBlock statements env
  let newEnv = M.insert (Nothing, id) res env
  return(newEnv)

errorReachableEnvBlock :: [Stmt ErrPos] -> ErrorReachableEnv -> Checker(ErrorReachable)
errorReachableEnvBlock [] _ = return(SurelyNo)
errorReachableEnvBlock (statement:rest) env = do
  reachable <- errorReachableEnvStatement statement env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> errorReachableEnvBlock rest env
    Unsure -> do
      restReachable <- errorReachableEnvBlock rest env
      case restReachable of
        SurelyYes -> return(SurelyYes)
        _ -> return(Unsure)
    RecheckRequired -> return(RecheckRequired)


errorReachableEnvStatement :: Stmt ErrPos -> ErrorReachableEnv -> Checker(ErrorReachable)
errorReachableEnvStatement (Empty _) _ = return(SurelyNo)
errorReachableEnvStatement (BStmt _ (Block _ statements)) env = errorReachableEnvBlock statements env
errorReachableEnvStatement (Decl _ _ []) _ = return(SurelyNo)
errorReachableEnvStatement (Decl errPos declType (item:rest)) env =
  case item of
    Init _ _ expr -> do
      reachable <- errorReachableEnvExpression expr env
      case reachable of
        SurelyYes -> return(SurelyYes)
        SurelyNo -> do
          errorReachableEnvStatement (Decl errPos declType rest) env
        Unsure -> do
          restReachable <- errorReachableEnvStatement (Decl errPos declType rest) env
          case restReachable of
            SurelyYes -> return(SurelyYes)
            _ -> return(Unsure)
        RecheckRequired -> return(RecheckRequired)
    _ -> return(SurelyNo)
errorReachableEnvStatement (Ass _ _ expr) env = errorReachableEnvExpression expr env
errorReachableEnvStatement (Incr _ _) _ = return(SurelyNo)
errorReachableEnvStatement (Decr _ _) _ = return(SurelyNo)
errorReachableEnvStatement (Ret _ expr) env = errorReachableEnvExpression expr env
errorReachableEnvStatement (VRet _) _ = return(SurelyNo)
errorReachableEnvStatement (Cond _ expression statement) env = do
  reachable <- errorReachableEnvExpression expression env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> do
      staticValueOfExpression <- evaluateConstantExpressionToBool expression
      case staticValueOfExpression of
        (Just True) -> errorReachableEnvStatement statement env
        (Just False) -> return(SurelyNo)
        (Nothing) -> errorReachableEnvStatement statement env
    Unsure -> do
      staticValueOfExpression <- evaluateConstantExpressionToBool expression
      case staticValueOfExpression of
        (Just True) -> do
          reachable' <- errorReachableEnvStatement statement env
          case(reachable') of
            (SurelyYes) -> return(SurelyYes)
            _ -> return(Unsure)
        _ -> return(Unsure)
    RecheckRequired ->
      return(RecheckRequired)
errorReachableEnvStatement (CondElse _ expression statement1 statement2) env = do
  reachable <- errorReachableEnvExpression expression env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> do
      staticValueOfExpression <- evaluateConstantExpressionToBool expression
      case staticValueOfExpression of
        (Just True) ->
          errorReachableEnvStatement statement1 env
        (Just False) ->
          errorReachableEnvStatement statement2 env
        (Nothing) -> do
          reachable' <- errorReachableEnvStatement statement1 env
          reachable'' <- errorReachableEnvStatement statement2 env
          case (reachable', reachable'') of
            (SurelyYes, SurelyYes) -> return(SurelyYes)
            (SurelyNo, SurelyNo) -> return(SurelyNo)
            (RecheckRequired, _) -> return(RecheckRequired)
            (_, RecheckRequired) -> return(RecheckRequired)
            _ -> return(Unsure)
    Unsure -> do
      staticValueOfExpression <- evaluateConstantExpressionToBool expression
      case staticValueOfExpression of
        (Just True) -> do
          reachable' <- errorReachableEnvStatement statement1 env
          case(reachable') of
            (SurelyYes) -> return(SurelyYes)
            _ -> return(Unsure)
        (Just False) -> do
          reachable' <- errorReachableEnvStatement statement2 env
          case(reachable') of
            (SurelyYes) -> return(SurelyYes)
            _ -> return(Unsure)
        (Nothing) -> do
          reachable' <- errorReachableEnvStatement statement1 env
          reachable'' <- errorReachableEnvStatement statement2 env
          case (reachable', reachable'') of
            (SurelyYes, SurelyYes) -> return(SurelyYes)
            _ -> return(Unsure)
    RecheckRequired ->
      return(RecheckRequired)
errorReachableEnvStatement (While _ expression statement) env = do
  reachable <- errorReachableEnvExpression expression env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> errorReachableEnvStatement statement env
    Unsure -> do
      reachable' <- errorReachableEnvStatement statement env
      case(reachable') of
        (SurelyYes) -> return(SurelyYes)
        _ -> return(Unsure)
    RecheckRequired ->
      return(RecheckRequired)
errorReachableEnvStatement (For _ _ _ expression statement) env = do
  reachable <- errorReachableEnvExpression expression env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> errorReachableEnvStatement statement env
    Unsure -> do
      reachable' <- errorReachableEnvStatement statement env
      case(reachable') of
        (SurelyYes) -> return(SurelyYes)
        _ -> return(Unsure)
    RecheckRequired ->
      return(RecheckRequired)
errorReachableEnvStatement (SExp _ expression) env = errorReachableEnvExpression expression env

errorReachableEnvExpressions :: [Expr ErrPos] -> ErrorReachableEnv -> Checker(ErrorReachable)
errorReachableEnvExpressions [] _ = return(SurelyNo)
errorReachableEnvExpressions (expr:rest) env = do
  reachable <- errorReachableEnvExpression expr env
  case reachable of
    SurelyYes -> return(SurelyYes)
    SurelyNo -> errorReachableEnvExpressions rest env
    Unsure -> do
      reachable' <- errorReachableEnvExpressions rest env
      case(reachable') of
        (SurelyYes) -> return(SurelyYes)
        _ -> return(Unsure)
    RecheckRequired ->
      return(RecheckRequired)


errorReachableEnvExpression :: Expr ErrPos -> ErrorReachableEnv -> Checker(ErrorReachable)
errorReachableEnvExpression (EVar _ _) _ = return(SurelyNo)
errorReachableEnvExpression (ELitInt _ _) _ = return(SurelyNo)
errorReachableEnvExpression (ELitTrue _) _ = return(SurelyNo)
errorReachableEnvExpression (ELitFalse _) _ = return(SurelyNo)
errorReachableEnvExpression (EApp _ id arguments) env = do
  case M.lookup (Nothing, id) env of
    (Just SurelyYes) -> return(SurelyYes)
    (Just RecheckRequired) -> return(RecheckRequired)
    (Just SurelyNo) -> errorReachableEnvExpressions arguments env
    (Just Unsure) -> do
      argumentsReachable <- errorReachableEnvExpressions arguments env
      case argumentsReachable of
        SurelyYes -> return(SurelyYes)
        _ -> return(Unsure)
    Nothing -> return(RecheckRequired)
errorReachableEnvExpression (EString _ _) _ = return(SurelyNo)
errorReachableEnvExpression (ENewArr _ _ sizeExpression) env = errorReachableEnvExpression sizeExpression env
errorReachableEnvExpression (ENewClass _ _) _ = return(SurelyNo)
errorReachableEnvExpression (EAttrFun _ _ _ _) _ = return(SurelyNo)
errorReachableEnvExpression (EAttrType _ _ _) _ = return(SurelyNo)
errorReachableEnvExpression (EArrAt _ _ indexExpression) env = errorReachableEnvExpression indexExpression env
errorReachableEnvExpression (ENullCast _ _) _ = return(SurelyNo)
errorReachableEnvExpression (ECast _ _ expr) env = errorReachableEnvExpression expr env
errorReachableEnvExpression (Neg _ expr) env = errorReachableEnvExpression expr env
errorReachableEnvExpression (Not _ expr) env = errorReachableEnvExpression expr env
errorReachableEnvExpression (EMul _ expr1 _ expr2) env = do
  reachable <- errorReachableEnvExpression expr1 env
  reachable' <- errorReachableEnvExpression expr2 env
  case (reachable, reachable') of
    (SurelyYes, _) -> return(SurelyYes)
    (SurelyNo, _) -> return(reachable')
    (RecheckRequired, _) -> return(RecheckRequired)
    (Unsure, SurelyYes) -> return(SurelyYes)
    (_, _) -> return(reachable')
errorReachableEnvExpression (EAdd _ expr1 _ expr2) env = do
  reachable <- errorReachableEnvExpression expr1 env
  reachable' <- errorReachableEnvExpression expr2 env
  case (reachable, reachable') of
    (SurelyYes, _) -> return(SurelyYes)
    (SurelyNo, _) -> return(reachable')
    (RecheckRequired, _) -> return(RecheckRequired)
    (Unsure, SurelyYes) -> return(SurelyYes)
    (_, _) -> return(reachable')
errorReachableEnvExpression (ERel _ expr1 _ expr2) env = do
  reachable <- errorReachableEnvExpression expr1 env
  reachable' <- errorReachableEnvExpression expr2 env
  case (reachable, reachable') of
    (SurelyYes, _) -> return(SurelyYes)
    (SurelyNo, _) -> return(reachable')
    (RecheckRequired, _) -> return(RecheckRequired)
    (Unsure, SurelyYes) -> return(SurelyYes)
    (_, _) -> return(reachable')
errorReachableEnvExpression (EAnd _ expr1 expr2) env = do
  reachable <- errorReachableEnvExpression expr1 env
  reachable' <- errorReachableEnvExpression expr2 env
  case (reachable, reachable') of
    (SurelyYes, _) -> return(SurelyYes)
    (SurelyNo, _) -> return(reachable')
    (RecheckRequired, _) -> return(RecheckRequired)
    (Unsure, SurelyYes) -> return(SurelyYes)
    (_, _) -> return(reachable')
errorReachableEnvExpression (EOr _ expr1 expr2) env = do
  reachable <- errorReachableEnvExpression expr1 env
  reachable' <- errorReachableEnvExpression expr2 env
  case (reachable, reachable') of
    (SurelyYes, _) -> return(SurelyYes)
    (SurelyNo, _) -> return(reachable')
    (RecheckRequired, _) -> return(RecheckRequired)
    (Unsure, SurelyYes) -> return(SurelyYes)
    (_, _) -> return(reachable')

defaultLibraryFunctionsEnv :: [(Ident, (Type ErrPos))] -> ErrorReachableEnv -> ErrorReachableEnv
defaultLibraryFunctionsEnv [] env = env
defaultLibraryFunctionsEnv ((Ident "error", _):rest) env =
  let partialMap = M.insert (Nothing, (Ident "error")) SurelyYes env in
    defaultLibraryFunctionsEnv rest partialMap
defaultLibraryFunctionsEnv ((id, _):rest) env =
  let partialMap = M.insert (Nothing, id) SurelyNo env in
    defaultLibraryFunctionsEnv rest partialMap