module Backend.CompilerData where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import Frontend.Grammar.ShowLatte
import Backend.Asm.Data

type Latte = (StateT Store (ExceptT String IO))

data Store = Store {
  functionsEnv :: FunEnv,
  classesEnv :: ClassEnv,
  variableEnv :: VarEnv,
  stringsEnv :: StrEnv,
  usedFunctions :: [String],
  labelCount :: Integer,
  nextVariable :: Integer,
  generated :: [Instruction],
  suf :: [String]
}

type FunEnv = M.Map Ident ([Arg ErrPos], Type ErrPos)
type ClassEnv = (M.Map Ident (Maybe Ident, Integer, MapOfClassAttributes, Bool))
type MapOfClassAttributes = M.Map (Ident, AttributeType) (ClassAttribute, Type ErrPos)
data ClassAttribute =
  ClassType Integer
  | ClassFunction [Arg ErrPos]
  deriving (Show)
data AttributeType =
  FunctionAttribute
  | DataAttribute
  deriving (Eq, Ord, Show, Read)
type VarEnv = M.Map Ident (Integer, Type ErrPos)
type StrEnv = M.Map String String
