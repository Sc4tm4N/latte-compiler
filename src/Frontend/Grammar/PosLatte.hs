module Frontend.Grammar.PosLatte where

import Prelude hiding (id)
import Frontend.Grammar.AbsLatte

getPosProgram :: Program a -> a
getPosProgram (Program pos _) = pos

getPosTopDef :: TopDef a -> a
getPosTopDef (FnDef pos _) = pos
getPosTopDef (ClassDef pos _ _) = pos
getPosTopDef (ExtendsClassDef pos _ _ _) = pos

getPosFunDef :: FunDef a -> a
getPosFunDef (FunDef pos _ _ _ _) = pos

getPosInClass :: ClassBody a -> a
getPosInClass (FunAttr pos _) = pos
getPosInClass (TypAttr pos _ _) = pos

getPosArg :: Arg a -> a
getPosArg (Arg pos _ _) = pos

getPosBlock :: Block a -> a
getPosBlock (Block pos _) = pos

getPosStmt :: Stmt a -> a
getPosStmt (Empty pos) = pos
getPosStmt (BStmt pos _) = pos
getPosStmt (Decl pos _ _) = pos
getPosStmt (Ass pos _ _) = pos
getPosStmt (Incr pos _) = pos
getPosStmt (Decr pos _) = pos
getPosStmt (Ret pos _) = pos
getPosStmt (VRet pos) = pos
getPosStmt (Cond pos _ _) = pos
getPosStmt (CondElse pos _ _ _) = pos
getPosStmt (While pos _ _) = pos
getPosStmt (SExp pos _) = pos
getPosStmt (For pos _ _ _ _) = pos

getPosItem :: Item a -> a
getPosItem (NoInit pos _) = pos
getPosItem (Init pos _ _) = pos

getPosType :: Type a -> a
getPosType (TInt pos) = pos
getPosType (TStr pos) = pos
getPosType (TBool pos) = pos
getPosType (TVoid pos) = pos
getPosType (TFun pos _ _) = pos
getPosType (TTab pos _) = pos
getPosType (TClass pos _) = pos

getPosExpr :: Expr a -> a
getPosExpr (EVar pos _) = pos
getPosExpr (ELitInt pos _) = pos
getPosExpr (ELitTrue pos) = pos
getPosExpr (ELitFalse pos) = pos
getPosExpr (EApp pos _ _) = pos
getPosExpr (EString pos _) = pos
getPosExpr (ENewArr pos _ _) = pos
getPosExpr (ENewClass pos _) = pos
getPosExpr (EAttrFun pos _ _ _) = pos
getPosExpr (EAttrType pos _ _) = pos
getPosExpr (EArrAt pos _ _) = pos
getPosExpr (Neg pos _) = pos
getPosExpr (Not pos _) = pos
getPosExpr (EMul pos _ _ _) = pos
getPosExpr (EAdd pos _ _ _) = pos
getPosExpr (ERel pos _ _ _) = pos
getPosExpr (EAnd pos _ _) = pos
getPosExpr (EOr pos _ _) = pos
getPosExpr (ENullCast pos _) = pos
getPosExpr (ECast pos _ _) = pos

getPosAddOp :: AddOp a -> a
getPosAddOp (Plus pos) = pos
getPosAddOp (Minus pos) = pos

getPosMulOp :: MulOp a -> a
getPosMulOp (Times pos) = pos
getPosMulOp (Div pos) = pos
getPosMulOp (Mod pos) = pos

getPosRelOp :: RelOp a -> a
getPosRelOp (LTH pos) = pos
getPosRelOp (LE pos) = pos
getPosRelOp (GTH pos) = pos
getPosRelOp (GE pos) = pos
getPosRelOp (EQU pos) = pos
getPosRelOp (NE pos) = pos