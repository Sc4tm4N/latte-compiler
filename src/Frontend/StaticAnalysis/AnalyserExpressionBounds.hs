module Frontend.StaticAnalysis.AnalyserExpressionBounds (analyzeExpressionBounds) where

import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(Nothing, Just))

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.Grammar.PosLatte (getPosExpr)
import Frontend.StaticAnalysis.Definitions (Checker)
import Frontend.StaticAnalysis.ErrorMessages (constCalculatorMaxBreach, constCalculatorMinBreach, tooLowArrayIndex,
  explicitDivisionByZero, explicitModuloDivisionByZero)


analyzeExpressionBounds :: Expr ErrPos -> Checker ()
analyzeExpressionBounds (ELitInt errPos val) = do
  _ <- isInInt32Bounds val errPos
  return ()
analyzeExpressionBounds expr = do
  maybeVal <- analyzeExpressionBoundsImpl expr
  case maybeVal of
    Just val -> do
      _ <- isInInt32Bounds val (getPosExpr expr)
      return()
    _ -> return()


-- expression constant value
analyzeExpressionBoundsImpl :: Expr ErrPos -> Checker (Maybe Integer)
analyzeExpressionBoundsImpl (EVar _ _) = return (Nothing)
analyzeExpressionBoundsImpl (ELitInt _ val) = return (Just val)
analyzeExpressionBoundsImpl (ELitTrue _) = return (Nothing)
analyzeExpressionBoundsImpl (ELitFalse _) = return (Nothing)
analyzeExpressionBoundsImpl (EApp _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EString _ _) = return (Nothing)
analyzeExpressionBoundsImpl (ENewArr _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (ENewClass _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EAttrFun _ _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EAttrType _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EArrAt errPos _ expr) = do
  maybeVal <- analyzeExpressionBoundsImpl expr
  case maybeVal of
    Just val -> if val < 0 then throwError $ tooLowArrayIndex errPos else return (Nothing)
    _ -> return (Nothing)
analyzeExpressionBoundsImpl (ENullCast _ _) = return (Nothing)
analyzeExpressionBoundsImpl (ECast _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (Neg errPos expr) = do
  maybeVal <- analyzeExpressionBoundsImpl expr
  case maybeVal of
    Just val -> isInInt32Bounds (negate val) errPos
    _ -> return (Nothing)
analyzeExpressionBoundsImpl (Not _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EMul errPos expr1 operand expr2) = do
  maybeVal1 <- analyzeExpressionBoundsImpl expr1
  maybeVal2 <- analyzeExpressionBoundsImpl expr2
  case (maybeVal1, maybeVal2) of
    (Just val1, Just val2) -> do
      _ <- isInInt32Bounds val1 errPos
      _ <- isInInt32Bounds val2 errPos
      case operand of
        Times _ -> isInInt32Bounds (val1 * val2) errPos
        Div _ ->
          if val2 == 0 then throwError $ (explicitDivisionByZero errPos) else isInInt32Bounds (val1 `div` val2) errPos
        Mod _ ->
          if val2 == 0 then throwError $ (explicitModuloDivisionByZero errPos) else isInInt32Bounds (val1 `mod` val2) errPos
    (Just val, _) -> do
      _ <- isInInt32Bounds val errPos
      case operand of
        Times _ -> if val == 0 then return (Just val) else return (Nothing)
        Div _ -> return (Nothing)
        Mod _ -> return (Nothing)
    (_, Just val) -> do
      _ <- isInInt32Bounds val errPos
      case operand of
        Times _ -> if val == 0 then return (Just val) else return (Nothing)
        Div _ -> if val == 0 then throwError $ (explicitDivisionByZero errPos) else return (Nothing)
        Mod _ -> if val == 0 then throwError $ (explicitModuloDivisionByZero errPos) else return (Nothing)
    _ -> return (Nothing)
analyzeExpressionBoundsImpl (EAdd errPos expr1 operand expr2) = do
  maybeVal1 <- analyzeExpressionBoundsImpl expr1
  maybeVal2 <- analyzeExpressionBoundsImpl expr2
  case (maybeVal1, maybeVal2) of
    (Just val1, Just val2) -> do
      _ <- isInInt32Bounds val1 errPos
      _ <- isInInt32Bounds val2 errPos
      case operand of
        Plus _ -> isInInt32Bounds (val1 + val2) errPos
        Minus _ -> isInInt32Bounds (val1 - val2) errPos
    (Just val, _) -> do
      _ <- isInInt32Bounds val errPos
      return (Nothing)
    (_, Just val) -> do
      _ <- isInInt32Bounds val errPos
      return (Nothing)
    _ -> return (Nothing)
analyzeExpressionBoundsImpl (ERel errPos expr1 _ expr2) = do
  maybeVal1 <- analyzeExpressionBoundsImpl expr1
  maybeVal2 <- analyzeExpressionBoundsImpl expr2
  case (maybeVal1, maybeVal2) of
    (Just val1, Just val2) -> do
      _ <- isInInt32Bounds val1 errPos
      _ <- isInInt32Bounds val2 errPos
      return(Nothing)
    (Just val, _) -> isInInt32Bounds val errPos
    (_, Just val) -> isInInt32Bounds val errPos
    _ -> return (Nothing)
analyzeExpressionBoundsImpl (EAnd _ _ _) = return (Nothing)
analyzeExpressionBoundsImpl (EOr _ _ _) = return (Nothing)


isInInt32Bounds :: Integer -> ErrPos -> Checker(Maybe Integer)
isInInt32Bounds val errPos
  | val < (negate 2147483648) = throwError $ constCalculatorMinBreach errPos
  | val > 2147483647 = throwError $ constCalculatorMaxBreach errPos
  | otherwise = return (Just val)
