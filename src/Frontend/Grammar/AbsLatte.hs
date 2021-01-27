module Frontend.Grammar.AbsLatte where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Read)
data Program a = Program a [TopDef a]
  deriving (Read)

instance Functor Program where
    fmap f x = case x of
        Program a topdefs -> Program (f a) (map (fmap f) topdefs)
data TopDef a
    = FnDef a (FunDef a)
    | ClassDef a Ident [ClassBody a]
    | ExtendsClassDef a Ident Ident [ClassBody a]
  deriving (Read)

instance Functor TopDef where
    fmap f x = case x of
        FnDef a fundef -> FnDef (f a) (fmap f fundef)
        ClassDef a ident classbodys -> ClassDef (f a) ident (map (fmap f) classbodys)
        ExtendsClassDef a ident1 ident2 classbodys -> ExtendsClassDef (f a) ident1 ident2 (map (fmap f) classbodys)
data ClassBody a = FunAttr a (FunDef a) | TypAttr a (Type a) Ident
  deriving (Read)

instance Functor ClassBody where
    fmap f x = case x of
        FunAttr a fundef -> FunAttr (f a) (fmap f fundef)
        TypAttr a type_ ident -> TypAttr (f a) (fmap f type_) ident
data FunDef a = FunDef a (Type a) Ident [Arg a] (Block a)
  deriving (Read)

instance Functor FunDef where
    fmap f x = case x of
        FunDef a type_ ident args block -> FunDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
data Arg a = Arg a (Type a) Ident
  deriving (Read)

instance Functor Arg where
    fmap f x = case x of
        Arg a type_ ident -> Arg (f a) (fmap f type_) ident
data Block a = Block a [Stmt a]
  deriving (Read)

instance Functor Block where
    fmap f x = case x of
        Block a stmts -> Block (f a) (map (fmap f) stmts)
data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a (Expr a) (Expr a)
    | Incr a (Expr a)
    | Decr a (Expr a)
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | SExp a (Expr a)
    | For a (Type a) Ident (Expr a) (Stmt a)
  deriving (Read)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Decl a type_ items -> Decl (f a) (fmap f type_) (map (fmap f) items)
        Ass a expr1 expr2 -> Ass (f a) (fmap f expr1) (fmap f expr2)
        Incr a expr -> Incr (f a) (fmap f expr)
        Decr a expr -> Decr (f a) (fmap f expr)
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)
        For a type_ ident expr stmt -> For (f a) (fmap f type_) ident (fmap f expr) (fmap f stmt)
data Item a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Read)

instance Functor Item where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)
data Type a
    = TInt a
    | TStr a
    | TBool a
    | TVoid a
    | TFun a (Type a) [Type a]
    | TTab a (Type a)
    | TClass a Ident
  deriving (Read)

instance Functor Type where
    fmap f x = case x of
        TInt a -> TInt (f a)
        TStr a -> TStr (f a)
        TBool a -> TBool (f a)
        TVoid a -> TVoid (f a)
        TFun a type_ types -> TFun (f a) (fmap f type_) (map (fmap f) types)
        TTab a type_ -> TTab (f a) (fmap f type_)
        TClass a ident -> TClass (f a) ident
data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a]
    | EString a String
    | ENewArr a (Type a) (Expr a)
    | ENewClass a (Type a)
    | EAttrFun a (Expr a) Ident [Expr a]
    | EAttrType a (Expr a) Ident
    | EArrAt a (Expr a) (Expr a)
    | ENullCast a Ident
    | ECast a Ident (Expr a)
    | Neg a (Expr a)
    | Not a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Read)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        EString a string -> EString (f a) string
        ENewArr a type_ expr -> ENewArr (f a) (fmap f type_) (fmap f expr)
        ENewClass a type_ -> ENewClass (f a) (fmap f type_)
        EAttrFun a expr ident exprs -> EAttrFun (f a) (fmap f expr) ident (map (fmap f) exprs)
        EAttrType a expr ident -> EAttrType (f a) (fmap f expr) ident
        EArrAt a expr1 expr2 -> EArrAt (f a) (fmap f expr1) (fmap f expr2)
        ENullCast a ident -> ENullCast (f a) ident
        ECast a ident expr -> ECast (f a) ident (fmap f expr)
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)
data AddOp a = Plus a | Minus a
  deriving (Read)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)
data MulOp a = Times a | Div a | Mod a
  deriving (Read)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)
data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Read)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)