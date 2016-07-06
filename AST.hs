module AST
    ( Lit(..)
    , Decl(..)
    , Expr(..)
    , Type(..)
    , Op(..)
    ) where


data Lit
    = LitString String
    | LitChar Char
    | LitInt Integer
    | LitFloat Float
    | LitBool Bool
    | LitArray [Expr]
    deriving (Eq, Show)


data Type
    = TypeString
    | TypeChar
    | TypeInt
    | TypeFloat
    | TypeBool
    | TypeGeneric String
    | TypeFunc [Type] Type
    | TypeList Type
    | TypeTuple [Type]
    deriving (Eq, Show)


type TypeDecl = ([Type], [Type])

data Decl
    = DeclFunc String Decl [Expr]
    | DeclSign [Type] Type
    deriving (Eq, Show)


data Expr
    = ExprParens Expr
    | ExprLit Lit
    | AOp Op Expr Expr
    | ExprStringInterpolation [Expr]
    | ExprVar String
    | ExprDiscard
    | ExprCase Expr Expr
    | ExprMatch [Expr]
    deriving (Eq, Show)

data Op
    = OpPlus
    deriving (Eq, Show)
