module Frontend.Grammar.ShowLatte where

import Prelude hiding (id)
import Frontend.Grammar.AbsLatte

instance Show Ident where
  show (Ident id) = id

instance Show (Program a) where
  show (Program _ td1) = show td1

instance Show (TopDef a) where
  show (FnDef _ fd1) = show fd1
  show (ClassDef _ id1 ic1) = (show id1) ++ " {\n" ++ (show ic1) ++ "\n}"
  show (ExtendsClassDef _ id1 id2 ic1) = (show id1) ++ " extends "
    ++ (show id2) ++ " {\n" ++ (show ic1) ++ "\n}"

instance Show (FunDef a) where
  show (FunDef _ t1 id1 a1 b1) = (show t1) ++ " " ++ (show id1) ++ "(" ++ (show a1)
    ++ ") " ++ (show b1)

instance Show (ClassBody a) where
  show (FunAttr _ fd1) = show fd1
  show (TypAttr _ t1 id1) = (show t1) ++ " " ++ (show id1) ++ ";"

instance Show (Arg a) where
  show (Arg _ t1 id1) = (show t1) ++ " " ++ (show id1)

instance Show (Block a) where
  show (Block _ sts1) = "{ " ++ (show sts1) ++ " }"

instance Show (Stmt a) where
  show (Empty _) = ";"
  show (BStmt _ b1) = "{ " ++ (show b1) ++ " }"
  show (Decl _ t1 its1) = (show t1) ++ " " ++ (show its1)
  show (Incr _ id1) = (show id1) ++ "++"
  show (Ass _ id1 e1) = (show id1) ++ " = " ++ (show e1)
  show (Decr _ id1) = (show id1) ++ "--"
  show (Ret _ e1) = "return " ++ (show e1)
  show (VRet _) = "return"
  show (Cond _ e1 s1) = "if (" ++ (show e1) ++ ") then " ++ (show s1)
  show (CondElse _ e1 s1 s2) = "if (" ++ (show e1) ++ ") then " ++ (show s1)
    ++ " else " ++ (show s2)
  show (While _ e1 s1) = "while (" ++ (show e1) ++ ") " ++ (show s1)
  show (For _ t1 id1 e1 s1) = "for (" ++ (show t1) ++ " " ++ (show id1)
    ++ " : " ++ (show e1) ++ ") " ++ (show s1)
  show (SExp _ e1) = (show e1) ++ ";"

instance Show (Item a) where
  show (NoInit _ id1) = (show id1)
  show (Init _ id1 e1) = (show id1) ++ " = " ++ (show e1)

instance Show (Type a) where
  show (TInt _) = "int"
  show (TStr _) = "string"
  show (TBool _) = "bool"
  show (TVoid _) = "void"
  show (TFun _ t1 ts1) = "(" ++ (show ts1) ++ ") -> " ++ (show t1)
  show (TTab _ ts1) = "(" ++ (show ts1) ++ ")[]"
  show (TClass _ id1) = "class " ++ (show id1)

instance Show (Expr a) where
  show (EVar _ id1) = show id1
  show (ELitInt _ i1) = show i1
  show (ELitTrue _) = "True"
  show (ELitFalse _) = "False"
  show (EApp _ id1 e1) = (show id1) ++ "(" ++ (show e1) ++ ")"
  show (EString _ s1) = s1
  show (ENewArr _ t1 e1) = "new " ++ (show t1) ++ "[" ++ (show e1) ++ "]"
  show (ENewClass _ t1) = "new " ++ (show t1)
  show (EAttrFun _ e1 id1 e2) = (show e1) ++ "." ++ (show id1) ++ "(" ++ (show e2) ++ ")"
  show (EAttrType _ e1 id1) = (show e1) ++ "." ++ (show id1)
  show (EArrAt _ e1 e2) = (show e1) ++ "[" ++ (show e2) ++ "]"
  show (ENullCast _ t) = "(" ++ (show t) ++ ")null"
  show (ECast _ t e) = "(" ++ (show t) ++ ")" ++ (show e)
  show (Neg _ e1) = "-" ++ (show e1)
  show (Not _ e1) = "!" ++ (show e1)
  show (EMul _ e1 op1 e2) = (show e1) ++ " " ++ (show op1) ++ " " ++ (show e2)
  show (EAdd _ e1 op1 e2) = (show e1) ++ " " ++ (show op1) ++ " " ++ (show e2)
  show (ERel _ e1 op1 e2) = (show e1) ++ " " ++ (show op1) ++ " " ++ (show e2)
  show (EAnd _ e1 e2) = (show e1) ++ " && " ++ (show e2)
  show (EOr _ e1 e2) = (show e1) ++ " || " ++ (show e2)

instance Show (AddOp a) where
  show (Plus _) = "+"
  show (Minus _) = "-"

instance Show (MulOp a) where
  show (Times _) = "*"
  show (Div _) = "/"
  show (Mod _) = "%"

instance Show (RelOp a) where
  show (LTH _) = "<"
  show (LE _) = "<="
  show (GTH _) = ">"
  show (GE _) = ">="
  show (EQU _) = "=="
  show (NE _) =
     "!="