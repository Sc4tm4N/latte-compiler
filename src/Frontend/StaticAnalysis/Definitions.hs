module Frontend.StaticAnalysis.Definitions (CheckerState, CheckerType, CheckerClassEnv, ClassAttributes, Checker,
  CheckerFunctionEnv, FunctionType) where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import qualified Data.Map as M (Map)

import Frontend.Grammar.AbsLatte (Ident, Type)
import Frontend.Grammar.EqLatte (ErrPos)

-- (id -> (type, was it declared in current block))
type CheckerState = (M.Map Ident CheckerType)
-- (type, was it declared in current block)
type CheckerType = (Type ErrPos, Bool)

-- (id -> (id -> (type)))
type CheckerFunctionEnv = (M.Map Ident FunctionType)
-- (id -> (type)
type FunctionType = (Type ErrPos)

-- (id -> (id -> (type)))
type CheckerClassEnv = (M.Map Ident (Maybe Ident, ErrPos, ClassAttributes))
-- fields: (id -> (type)) , functions: (id -> (type))
type ClassAttributes = (M.Map Ident (Type ErrPos), M.Map Ident (Type ErrPos))

type Checker = ReaderT (CheckerState, CheckerFunctionEnv, CheckerClassEnv) (ExceptT String IO)
