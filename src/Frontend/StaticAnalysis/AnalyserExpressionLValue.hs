module Frontend.StaticAnalysis.AnalyserExpressionLValue (analyzeExpressionLValue) where

import Control.Monad.Except (throwError)

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.StaticAnalysis.Definitions (Checker)
import Frontend.StaticAnalysis.ErrorMessages (assigningLValue)


analyzeExpressionLValue :: Expr ErrPos -> Checker ()
analyzeExpressionLValue (EVar _ _) = return()
analyzeExpressionLValue (ELitInt errPos _) = notLValue errPos
analyzeExpressionLValue (ELitTrue errPos) = notLValue errPos
analyzeExpressionLValue (ELitFalse errPos) = notLValue errPos
analyzeExpressionLValue (EApp errPos _ _) = notLValue errPos
analyzeExpressionLValue (EString errPos _) = notLValue errPos
analyzeExpressionLValue (ENewArr errPos _ _) = notLValue errPos
analyzeExpressionLValue (ENewClass errPos _) = notLValue errPos
analyzeExpressionLValue (EAttrFun errPos _ _ _) = notLValue errPos
analyzeExpressionLValue (EAttrType _ _ _) = return ()
analyzeExpressionLValue (EArrAt _ _ _) = return ()
analyzeExpressionLValue (ENullCast errPos _) = notLValue errPos
analyzeExpressionLValue (ECast _ _ _) = return ()
analyzeExpressionLValue (Neg errPos _) = notLValue errPos
analyzeExpressionLValue (Not errPos _) = notLValue errPos
analyzeExpressionLValue (EMul errPos _ _ _) = notLValue errPos
analyzeExpressionLValue (EAdd errPos _ _ _) = notLValue errPos
analyzeExpressionLValue (ERel errPos _ _ _) = notLValue errPos
analyzeExpressionLValue (EAnd errPos _ _) = notLValue errPos
analyzeExpressionLValue (EOr errPos _ _) = notLValue errPos

notLValue :: ErrPos -> Checker()
notLValue errPos = throwError $ assigningLValue errPos