module Frontend.StaticAnalysis.AnalyserExpressionAssignability (rightAssignableToLeft,
  rightAssignableToLeftLists, canAssign) where

import Control.Monad.Reader (ask)
import qualified Data.Map as M (lookup)

import Frontend.StaticAnalysis.Definitions (CheckerClassEnv)

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte (ErrPos)
import Frontend.StaticAnalysis.Definitions (Checker)


rightAssignableToLeft :: Type ErrPos -> Type ErrPos -> ErrPos -> Checker(Bool)
rightAssignableToLeft (TInt _) (TInt _) _ = return True
rightAssignableToLeft (TStr _) (TStr _) _ = return True
rightAssignableToLeft (TBool _) (TBool _) _ = return True
rightAssignableToLeft (TVoid _) (TVoid _) _ = return True
rightAssignableToLeft (TFun _ returnType1 parameters1) (TFun _ returnType2 parameters2) errPos = do
  isAssignable <- rightAssignableToLeft returnType1 returnType2 errPos
  case isAssignable of
    True -> rightAssignableToLeftLists parameters1 parameters2 errPos
    _    -> return False
rightAssignableToLeft (TTab _ arrayType1) (TTab _ arrayType2) errPos = rightAssignableToLeft arrayType1 arrayType2 errPos
rightAssignableToLeft (TClass _ id1) (TClass _ id2) errPos = do
  (_, _, classEnv) <- ask
  case (M.lookup id1 classEnv, M.lookup id2 classEnv) of
    (Nothing, _) -> return False
    (_, Nothing) -> return False
    (_, Just (Nothing, _, _)) ->
      return $ id1 == id2
    (_, Just (Just parentClassId, _, _)) ->
      if id1 == id2 then
        return True
      else
        rightAssignableToLeft (TClass errPos id1) (TClass errPos parentClassId) errPos
rightAssignableToLeft _ _ _ = return False

rightAssignableToLeftLists :: [Type ErrPos] -> [Type ErrPos] -> ErrPos -> Checker(Bool)
rightAssignableToLeftLists [] [] _ = return (True)
rightAssignableToLeftLists (type1:restTypes1) (type2:restTypes2) errPos = do
  isAssignable <- rightAssignableToLeft type1 type2 errPos
  case isAssignable of
    True -> rightAssignableToLeftLists restTypes1 restTypes2 errPos
    _    -> return False
rightAssignableToLeftLists (_:_) [] _ = return False
rightAssignableToLeftLists [] (_:_) _ = return False

canAssign :: CheckerClassEnv -> (Type ErrPos) -> (Type ErrPos) -> Bool
canAssign _ (TInt _) (TInt _) = True
canAssign _ (TStr _) (TStr _) = True
canAssign _ (TBool _) (TBool _) = True
canAssign _ (TVoid _) (TVoid _) = True
canAssign classEnv (TFun _ returnType1 parameters1) (TFun _ returnType2 parameters2) =
  case canAssign classEnv returnType1 returnType2 of
    True -> canAssignLists classEnv parameters1 parameters2
    False -> False
canAssign classEnv (TTab _ arrayType1) (TTab _ arrayType2) = canAssign classEnv arrayType1 arrayType2
canAssign classEnv (TClass errPos1 id1) (TClass errPos2 id2) =
  case (M.lookup id1 classEnv, M.lookup id2 classEnv) of
    (Nothing, _) -> False
    (_, Nothing) -> False
    (_, Just (Nothing, _, _)) ->
      if id1 == id2 then
        True
      else
        False
    (_, Just (Just parentClassId, _, _)) ->
      if id1 == id2 then
        True
      else
        canAssign classEnv (TClass errPos1 id1) (TClass errPos2 parentClassId)
canAssign _ _ _ = False

canAssignLists :: CheckerClassEnv -> [(Type ErrPos)] -> [(Type ErrPos)] -> Bool
canAssignLists _ [] [] = True
canAssignLists classEnv (type1:restTypes1) (type2:restTypes2) =
  case canAssign classEnv type1 type2 of
    True -> canAssignLists classEnv restTypes1 restTypes2
    False -> False
canAssignLists _ (_:_) [] = False
canAssignLists _ [] (_:_) = False