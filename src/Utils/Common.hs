module Utils.Common where

import Prelude hiding (id)
import Frontend.Grammar.AbsLatte
import Frontend.Grammar.EqLatte

defaultLibraryFunctions :: [(Ident, (Type ErrPos))]
defaultLibraryFunctions = [
  (Ident "printInt", (TFun Nothing (TVoid Nothing) [TInt Nothing])),
  (Ident "printString", (TFun Nothing (TVoid Nothing) [TStr Nothing])),
  (Ident "readInt", (TFun Nothing (TInt Nothing) [])),
  (Ident "readString", (TFun Nothing (TStr Nothing) [])),
  (Ident "error", (TFun Nothing (TVoid Nothing) [])),
  (Ident "calloc", (TFun Nothing (TInt Nothing) [TInt Nothing, TInt Nothing])),
  (Ident "memcpy", (TFun Nothing (TInt Nothing) [TInt Nothing, TInt Nothing, TInt Nothing])),
  (Ident "strcmp", (TFun Nothing (TInt Nothing) [TStr Nothing, TStr Nothing])),
  (Ident "concat", (TFun Nothing (TStr Nothing) [TStr Nothing, TStr Nothing]))]

backendDefaultLibraryFunctions :: [(Ident, ([Arg ErrPos], Type ErrPos))]
backendDefaultLibraryFunctions = [(Ident "printInt", ([(Arg Nothing (TInt Nothing) (Ident "i"))], TVoid Nothing)),
  (Ident "printString", ([(Arg Nothing (TStr Nothing) (Ident "s"))], TVoid Nothing)),
  (Ident "readInt", ([], TInt Nothing)),
  (Ident "readString", ([], TStr Nothing)),
  (Ident "error", ([], TVoid Nothing)),
  (Ident "calloc", ([Arg Nothing (TInt Nothing) (Ident "mnemb"),
    Arg Nothing (TInt Nothing) (Ident "size")], TInt Nothing)),
  (Ident "memcpy", ([Arg Nothing (TInt Nothing) (Ident "dest"),
    Arg Nothing (TInt Nothing) (Ident "src"),
    Arg Nothing (TInt Nothing) (Ident "n")], TInt Nothing)),
  (Ident "strcmp", ([Arg Nothing (TStr Nothing) (Ident "s1"),
      Arg Nothing (TStr Nothing) (Ident "s2")], TInt Nothing)),
  (Ident "concat", ([(Arg Nothing (TStr Nothing) (Ident "s")),
    (Arg Nothing (TStr Nothing) (Ident "z"))], TStr Nothing))]

createDecls :: ErrPos -> Type ErrPos -> [Item ErrPos] -> [Stmt ErrPos] -> [Stmt ErrPos]
createDecls _ _ [] acc = acc
createDecls errPos t (i:it) acc = createDecls errPos t it ((Decl errPos t [i]):acc)

reverseStatementsAndRegularize :: [Stmt ErrPos] -> [Stmt ErrPos]
reverseStatementsAndRegularize statements = _internalImplementation (reverse statements) [] where
  _internalImplementation [] acc = acc
  _internalImplementation ((Decl pos t its):stmts) acc =
    _internalImplementation stmts ((createDecls pos t (reverse its) []) ++ acc)
  _internalImplementation ((BStmt pos (Block pos1 b)):stmts) acc =
    _internalImplementation stmts ((BStmt pos (Block pos1 (reverseStatementsAndRegularize b))):acc)
  _internalImplementation ((While pos e1 stmt):stmts) acc =
    _internalImplementation stmts ((While pos e1 (BStmt pos (Block pos (reverseStatementsAndRegularize [stmt])))):acc)
  _internalImplementation ((For pos t id e1 stmt):stmts) acc =
    _internalImplementation stmts ((For pos t id e1 (BStmt pos (Block pos (reverseStatementsAndRegularize [stmt])))):acc)
  _internalImplementation ((Cond pos e1 stmt):stmts) acc =
    _internalImplementation stmts ((Cond pos e1 (BStmt pos (Block pos (reverseStatementsAndRegularize [stmt])))):acc)
  _internalImplementation ((CondElse pos e1 stmt1 stmt2):stmts) acc =
    _internalImplementation stmts ((CondElse pos e1 (BStmt pos (Block pos (reverseStatementsAndRegularize [stmt1])))
      (BStmt pos (Block pos (reverseStatementsAndRegularize [stmt2])))):acc)
  _internalImplementation (stmt:stmts) acc = _internalImplementation stmts (stmt:acc)

