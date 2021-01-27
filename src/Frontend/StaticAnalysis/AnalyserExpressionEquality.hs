module Frontend.StaticAnalysis.AnalyserExpressionEquality (areSameTypes) where

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.StaticAnalysis.Definitions (Checker)


areSameTypes :: Type ErrPos -> Type ErrPos -> ErrPos -> Checker(Bool)
areSameTypes (TInt _) (TInt _) _ = return True
areSameTypes (TStr _) (TStr _) _ = return True
areSameTypes (TBool _) (TBool _) _ = return True
areSameTypes (TVoid _) (TVoid _) _ = return True
areSameTypes (TFun _ returnType1 parameters1) (TFun _ returnType2 parameters2) errPos = do
  areEqual <- areSameTypes returnType1 returnType2 errPos
  case areEqual of
    True -> areSameTypesLists parameters1 parameters2 errPos
    _    -> return False
areSameTypes (TTab _ arrayType1) (TTab _ arrayType2) errPos = areSameTypes arrayType1 arrayType2 errPos
areSameTypes (TClass _ id1) (TClass _ id2) _ = return (id1 == id2)
areSameTypes _ _ _ = return False

areSameTypesLists :: [Type ErrPos] -> [Type ErrPos] -> ErrPos -> Checker(Bool)
areSameTypesLists [] [] _ = return (True)
areSameTypesLists (type1:restTypes1) (type2:restTypes2) errPos = do
  isAssignable <- areSameTypes type1 type2 errPos
  case isAssignable of
    True -> areSameTypesLists restTypes1 restTypes2 errPos
    _    -> return False
areSameTypesLists (_:_) [] _ = return False
areSameTypesLists [] (_:_) _ = return False
