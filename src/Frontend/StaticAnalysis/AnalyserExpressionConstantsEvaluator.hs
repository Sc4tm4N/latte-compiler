module Frontend.StaticAnalysis.AnalyserExpressionConstantsEvaluator (evaluateConstantExpressionToBool) where

import Prelude hiding (id)
import Data.Maybe (Maybe(Nothing, Just))

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.StaticAnalysis.Definitions (Checker)


evaluateConstantExpressionToBool :: Expr ErrPos -> Checker (Maybe Bool)
evaluateConstantExpressionToBool expr = do
  maybeConstVal <- evaluateConstantExpressions expr
  case maybeConstVal of
    (Just constBool, _, _, _) -> return (Just constBool)
    _ -> return (Nothing)

evaluateConstantExpressions :: Expr ErrPos -> Checker (Maybe Bool, Maybe Integer, Maybe String, Maybe Ident)
evaluateConstantExpressions (EVar _ id) = return (Nothing, Nothing, Nothing, Just id)
evaluateConstantExpressions (ELitInt _ val) = return (Nothing, Just val, Nothing, Nothing)
evaluateConstantExpressions (ELitTrue _) = return (Just True, Nothing, Nothing, Nothing)
evaluateConstantExpressions (ELitFalse _) = return (Just False, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EApp _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EString _ val) = return (Nothing, Nothing, Just val, Nothing)
evaluateConstantExpressions (ENewArr _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (ENewClass _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EAttrFun _ _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EAttrType _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EArrAt _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (ENullCast _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (ECast _ _ _) = return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (Neg _ expr) = do
  maybeEvaluations <- evaluateConstantExpressions expr
  case maybeEvaluations of
    (_, Just intVal, _, _) ->
      return (Nothing, Just (negate intVal), Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (Not _ expr) = do
  maybeEvaluations <- evaluateConstantExpressions expr
  case maybeEvaluations of
    (Just boolVal, _, _, _) -> return (Just (not boolVal), Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EMul _ expr1 _ expr2) = do
  maybeEvaluations1 <- evaluateConstantExpressions expr1
  maybeEvaluations2 <- evaluateConstantExpressions expr2
  case (maybeEvaluations1, maybeEvaluations2) of
    ((_, Just intVal1, _, _), (_, Just intVal2, _, _)) -> return (Nothing, Just $ intVal1 * intVal2, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EAdd _ expr1 _ expr2) = do
  maybeEvaluations1 <- evaluateConstantExpressions expr1
  maybeEvaluations2 <- evaluateConstantExpressions expr2
  case (maybeEvaluations1, maybeEvaluations2) of
    ((_, Just intVal1, _, _), (_, Just intVal2, _, _)) -> return (Nothing, Just $ intVal1 + intVal2, Nothing, Nothing)
    ((_, _, Just stringVal1, _), (_, _, Just stringVal2, _)) ->
      return (Nothing, Nothing, Just $ stringVal1 ++ stringVal2, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (ERel _ expr1 operation expr2) = do
  maybeEvaluations1 <- evaluateConstantExpressions expr1
  maybeEvaluations2 <- evaluateConstantExpressions expr2
  case (maybeEvaluations1, maybeEvaluations2) of
    ((Just boolVal1, _, _, _), (Just boolVal2, _, _, _)) -> do
      boolResult <- compareConstBools boolVal1 boolVal2 operation
      return(Just boolResult, Nothing, Nothing, Nothing)
    ((_, Just intVal1, _, _), (_, Just intVal2, _, _)) -> do
      boolResult <- compareConstIntegers intVal1 intVal2 operation
      return(Just boolResult, Nothing, Nothing, Nothing)
    ((_, _, Just stringVal1, _), (_, _, Just stringVal2, _)) -> do
      boolResult <- compareConstStrings stringVal1 stringVal2 operation
      return(Just boolResult, Nothing, Nothing, Nothing)
    ((_, _, _, Just (Ident id1)), (_, _, _, Just (Ident id2))) -> do
      boolResult <- compareVariablesIds id1 id2 operation
      return(boolResult, Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EAnd _ expr1 expr2) = do
  maybeEvaluations1 <- evaluateConstantExpressions expr1
  maybeEvaluations2 <- evaluateConstantExpressions expr2
  case (maybeEvaluations1, maybeEvaluations2) of
    ((Just boolVal1, _, _, _), (Just boolVal2, _, _, _)) -> return (Just $ boolVal1 && boolVal2, Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)
evaluateConstantExpressions (EOr _ expr1 expr2) = do
  maybeEvaluations1 <- evaluateConstantExpressions expr1
  maybeEvaluations2 <- evaluateConstantExpressions expr2
  case (maybeEvaluations1, maybeEvaluations2) of
    ((Just boolVal1, _, _, _), (Just boolVal2, _, _, _)) -> return (Just $ boolVal1 || boolVal2, Nothing, Nothing, Nothing)
    ((_, _, _, _), (Just boolVal, _, _, _)) -> return (Just boolVal, Nothing, Nothing, Nothing)
    ((Just boolVal, _, _, _), (_, _, _, _)) -> return (Just boolVal, Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing)


compareConstIntegers :: Integer -> Integer -> RelOp a -> Checker(Bool)
compareConstIntegers x y (LTH _) = return (x < y)
compareConstIntegers x y (LE  _) = return (x <=  y)
compareConstIntegers x y (GTH _) = return (x > y)
compareConstIntegers x y (GE  _) = return (x >=  y)
compareConstIntegers x y (EQU _) = return (x == y)
compareConstIntegers x y (NE  _) = return (x /= y)

compareConstBools :: Bool -> Bool -> RelOp a -> Checker(Bool)
compareConstBools x y (LTH _) = return (x < y)
compareConstBools x y (LE  _) = return (x <=  y)
compareConstBools x y (GTH _) = return (x > y)
compareConstBools x y (GE  _) = return (x >=  y)
compareConstBools x y (EQU _) = return (x == y)
compareConstBools x y (NE  _) = return (x /= y)

compareConstStrings :: String -> String -> RelOp a -> Checker(Bool)
compareConstStrings x y (LTH _) = return (x < y)
compareConstStrings x y (LE  _) = return (x <=  y)
compareConstStrings x y (GTH _) = return (x > y)
compareConstStrings x y (GE  _) = return (x >=  y)
compareConstStrings x y (EQU _) = return (x == y)
compareConstStrings x y (NE  _) = return (x /= y)

compareVariablesIds :: String -> String -> RelOp a -> Checker(Maybe Bool)
compareVariablesIds x y (EQU _)
  | x == y    = return(Just True)
  | otherwise = return(Nothing)
compareVariablesIds x y (LE _)
  | x == y    = return(Just True)
  | otherwise = return(Nothing)
compareVariablesIds x y (GE _)
  | x == y    = return(Just True)
  | otherwise = return(Nothing)
compareVariablesIds x y (NE _)
  | x == y    = return(Just False)
  | otherwise = return(Nothing)
compareVariablesIds _ _ _ = return(Nothing)