module Frontend.Grammar.EqLatte where

import Frontend.Grammar.AbsLatte

type ErrPos = Maybe (Int, Int)

mshow :: ErrPos -> String
mshow Nothing = "Unknown position: "
mshow (Just (i, _)) = "Line " ++ (show i) ++ ": "

bshow :: ErrPos -> String
bshow Nothing = "Unknown position: "
bshow (Just (i, _)) = "at line " ++ (show i)

instance Eq (Program a) where
  (Program _ td1) == (Program _ td2) = td1 == td2
  a /= b = not (a == b)

instance Eq (TopDef a) where
  (FnDef _ fd1) == (FnDef _ fd2) = fd1 == fd2
  (ClassDef _ id1 ic1) == (ClassDef _ id2 ic2) = id1 == id2 && ic1 == ic2
  (ExtendsClassDef _ id1 id2 ic1) == (ExtendsClassDef _ id3 id4 ic2) =
    id1 == id3 && id2 == id4 && ic1 == ic2
  _ == _ = False
  a /= b = not (a == b)

instance Eq (FunDef a) where
  (FunDef _ t1 id1 a1 b1) == (FunDef _ t2 id2 a2 b2) =
    t1 == t2 && id1 == id2 && a1 == a2 && b1 == b2
  a /= b = not (a == b)

instance Eq (ClassBody a) where
  (FunAttr _ fd1) == (FunAttr _ fd2) = fd1 == fd2
  (TypAttr _ t1 id1) == (TypAttr _ t2 id2) = t1 == t2 && id1 == id2
  (TypAttr _ _ _) == (FunAttr _ _) = False
  (FunAttr _ _) == (TypAttr _ _ _) = False
  a /= b = not (a == b)

instance Eq (Arg a) where
  (Arg _ t1 id1) == (Arg _ t2 id2) = t1 == t2 && id1 == id2
  a /= b = not (a == b)

instance Eq (Block a) where
  (Block _ sts1) == (Block _ sts2) = sts1 == sts2
  a /= b = not (a == b)

instance Eq (Stmt a) where
  (Empty _) == (Empty _) = True
  (BStmt _ b1) == (BStmt _ b2) = b1 == b2
  (Decl _ t1 its1) == (Decl _ t2 its2) = t1 == t2 && its1 == its2
  (Ass _ id1 e1) == (Ass _ id2 e2) = id1 == id2 && e1 == e2
  (Incr _ id1) == (Incr _ id2) = id1 == id2
  (Decr _ id1) == (Decr _ id2) = id1 == id2
  (Ret _ e1) == (Ret _ e2) = e1 == e2
  (VRet _) == (VRet _) = True
  (Cond _ e1 s1) == (Cond _ e2 s2) = e1 == e2 && s1 == s2
  (CondElse _ e1 s1 s2) == (CondElse _ e2 s3 s4) = e1 == e2 && s1 == s3 && s2 == s4
  (While _ e1 s1) == (While _ e2 s2) = e1 == e2 && s1 == s2
  (For _ t1 id1 e1 s1) == (For _ t2 id2 e2 s2) =
    t1 == t2 && id1 == id2 && e1 == e2 && s1 == s2
  (SExp _ e1) == (SExp _ e2) = e1 == e2
  _ == _ = False
  a /= b = not (a == b)

instance Eq (Item a) where
  (NoInit _ id1) == (NoInit _ id2) = id1 == id2
  (Init _ id1 e1) == (Init _ id2 e2) = id1 == id2 && e1 == e2
  _ == _ = False
  a /= b = not (a == b)

instance Eq (Type a) where
  (TInt _) == (TInt _) = True
  (TStr _) == (TStr _) = True
  (TBool _) == (TBool _) = True
  (TVoid _) == (TVoid _) = True
  (TFun _ t1 ts1) == (TFun _ t2 ts2) = t1 == t2 && ts1 == ts2
  (TTab _ ts1) == (TTab _ ts2) = ts1 == ts2
  (TClass _ id1) == (TClass _ id2) = id1 == id2
  _ == _ = False
  a /= b = not (a == b)

instance Eq (Expr a) where
  (EVar _ id1) == (EVar _ id2) = id1 == id2
  (ELitInt _ i1) == (ELitInt _ i2) = i1 == i2
  (ELitTrue _) == (ELitTrue _) = True
  (ELitFalse _) == (ELitFalse _) = True
  (EApp _ id1 e1) == (EApp _ id2 e2) = id1 == id2 && e1 == e2
  (EString _ s1) == (EString _ s2) = s1 == s2
  (ENewArr _ t1 e1) == (ENewArr _ t2 e2) = t1 == t2 && e1 == e2
  (ENewClass _ t1) == (ENewClass _ t2) = t1 == t2
  (EAttrFun _ e1 id1 e2) == (EAttrFun _ e3 id2 e4) =
    e1 == e3 && id1 == id2 && e2 == e4
  (EAttrType _ e1 id1) == (EAttrType _ e2 id2) = e1 == e2 && id1 == id2
  (EArrAt _ e1 e2) == (EArrAt _ e3 e4) = e1 == e3 && e2 == e4
  (ENullCast _ t1) == (ENullCast _ t2) = t1 == t2
  (ECast _ t1 e1) == (ECast _ t2 e2) = t1 == t2 && e1 == e2
  (Neg _ e1) == (Neg _ e2) = e1 == e2
  (Not _ e1) == (Not _ e2) = e1 == e2
  (EMul _ e1 op1 e2) == (EMul _ e3 op2 e4) = e1 == e3 && op1 == op2 && e2 == e4
  (EAdd _ e1 op1 e2) == (EAdd _ e3 op2 e4) = e1 == e3 && op1 == op2 && e2 == e4
  (ERel _ e1 op1 e2) == (ERel _ e3 op2 e4) = e1 == e3 && op1 == op2 && e2 == e4
  (EAnd _ e1 e2) == (EAnd _ e3 e4) = e1 == e3 && e2 == e4
  (EOr _ e1 e2) == (EOr _ e3 e4) = e1 == e3 && e2 == e4
  _ == _ = False
  a /= b = not (a == b)

instance Eq (AddOp a) where
  (Plus _) == (Plus _) = True
  (Minus _) == (Minus _) = True
  _ == _ = False
  a /= b = not (a == b)

instance Eq (MulOp a) where
  (Times _) == (Times _) = True
  (Div _) == (Div _) = True
  (Mod _) == (Mod _) = True
  _ == _ = False
  a /= b = not (a == b)

instance Eq (RelOp a) where
  (LTH _) == (LTH _) = True
  (LE _) == (LE _) = True
  (GTH _) == (GTH _) = True
  (GE _) == (GE _) = True
  (EQU _) == (EQU _) = True
  (NE _) == (NE _) = True
  _ == _ = False
  a /= b = not (a == b)