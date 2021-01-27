module Backend.Utils (gatherStrings, countLocalVariables) where

import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte
import qualified Data.Map as M

gatherStrings :: [TopDef ErrPos] -> (M.Map String String)
gatherStrings topDefs = fst $ _gatherStrings topDefs (M.fromList [("\"\"", ".Str0")], 1)

_gatherStrings :: [TopDef ErrPos] -> (M.Map String String, Integer) -> (M.Map String String, Integer)
_gatherStrings [] accumulator = accumulator
_gatherStrings ((FnDef _ (FunDef _ _ _ _ (Block _ statements))):rest) accumulator =
  _gatherStrings rest (foldl gatherStringsFromStmt accumulator statements)
_gatherStrings ((ClassDef _ _ classBody):rest) accumulator =
  _gatherStrings rest (foldl gatherStringsFromClass accumulator classBody)
_gatherStrings ((ExtendsClassDef _ _ _ classBody):rest) accumulator =
  _gatherStrings rest (foldl gatherStringsFromClass accumulator classBody)

gatherStringsFromClass :: (M.Map String String, Integer) -> ClassBody ErrPos -> (M.Map String String, Integer)
gatherStringsFromClass accumulator (FunAttr errPos funBody) = _gatherStrings [FnDef errPos funBody] accumulator
gatherStringsFromClass accumulator _ = accumulator

gatherStringsFromStmt :: (M.Map String String, Integer) -> Stmt ErrPos -> (M.Map String String, Integer)
gatherStringsFromStmt accumulator (BStmt _ (Block _ statements)) = foldl gatherStringsFromStmt accumulator statements
gatherStringsFromStmt accumulator (Decl _ _ items) = foldl gatherStringsItem accumulator items where
  gatherStringsItem accumulator' (Init _ _ expression) = gatherStringsFromExp accumulator' expression
  gatherStringsItem accumulator' _ = accumulator'
gatherStringsFromStmt accumulator (Ass _ _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromStmt accumulator (Ret _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromStmt accumulator (Cond _ conditionExpression trueStatements) =
  gatherStringsFromStmt (gatherStringsFromExp accumulator conditionExpression) trueStatements
gatherStringsFromStmt accumulator (CondElse _ conditionExpression trueStatements falseStatements) =
  gatherStringsFromStmt (gatherStringsFromStmt (gatherStringsFromExp accumulator conditionExpression) trueStatements) falseStatements
gatherStringsFromStmt accumulator (While _ conditionExpression statements) =
  gatherStringsFromStmt (gatherStringsFromExp accumulator conditionExpression) statements
gatherStringsFromStmt accumulator (SExp _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromStmt accumulator (For _ _ _ expression statements) =
  gatherStringsFromStmt (gatherStringsFromExp accumulator expression) statements
gatherStringsFromStmt accumulator _ = accumulator

gatherStringsFromExp :: (M.Map String String, Integer) -> Expr ErrPos -> (M.Map String String, Integer)
gatherStringsFromExp accumulator (EApp _ _ expressions) = foldl gatherStringsFromExp accumulator expressions
gatherStringsFromExp (strings, idx) (EString _ stringContent) =
  (M.insert stringContent (".Str" ++ (show idx)) strings, idx + 1)
gatherStringsFromExp accumulator (ENewArr _ _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromExp accumulator (EAttrFun _ expression _ expressions) =
  foldl gatherStringsFromExp (gatherStringsFromExp accumulator expression) expressions
gatherStringsFromExp accumulator (EAttrType _ expression _) = gatherStringsFromExp accumulator expression
gatherStringsFromExp accumulator (EArrAt _ expression1 expression2) =
  gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator (ECast _ _ expression) =
  gatherStringsFromExp accumulator expression
gatherStringsFromExp accumulator (Neg _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromExp accumulator (Not _ expression) = gatherStringsFromExp accumulator expression
gatherStringsFromExp accumulator (EMul _ expression1 _ expression2) = gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator (ERel _ expression1 _ expression2) = gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator (EAdd _ expression1 _ expression2) = gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator (EAnd _ expression1 expression2) = gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator (EOr _ expression1 expression2) = gatherStringsFromExp (gatherStringsFromExp accumulator expression1) expression2
gatherStringsFromExp accumulator _ = accumulator

countLocalVariables :: [Stmt ErrPos] -> Integer
countLocalVariables statements = _countLocalVariables statements 0 where
  _countLocalVariables [] accumulator = accumulator
  _countLocalVariables ((Decl _ _ items):rest) accumulator =
    _countLocalVariables rest (accumulator + (toInteger $ length items))
  _countLocalVariables ((BStmt _ (Block _ block)):rest) accumulator =
    _countLocalVariables rest ((countLocalVariables block) + accumulator)
  _countLocalVariables ((Cond _ _ block):rest) accumulator =
    _countLocalVariables rest ((countLocalVariables [block]) + accumulator)
  _countLocalVariables ((CondElse _ _ block1 block2):rest) accumulator =
    _countLocalVariables rest ((countLocalVariables [block1, block2]) + accumulator)
  _countLocalVariables ((While _ _ block):rest) accumulator =
    _countLocalVariables rest ((countLocalVariables [block]) + accumulator)
  _countLocalVariables ((For _ _ _ _ block):rest) accumulator =
    _countLocalVariables rest ((countLocalVariables [block]) + accumulator + 2) -- (int a : new int[10])
  _countLocalVariables (_:rest) accumulator = _countLocalVariables rest accumulator