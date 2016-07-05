module AST
    ( Lit(..)
    , Decl(..)
    , Expr(..)
    , Type(..)
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
    | TypeList Type
    | TypeTuple [Type]
    deriving (Eq, Show)


type TypeDecl = ([Type], [Type])

data Decl
    = DeclFunc String Decl [Expr]
    | DeclSign [Decl] Decl
    | DeclType Type
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    | ExprStringInterpolation [Expr]
    | ExprVar String
    | ExprDiscard
    | ExprCase Expr Expr
    | ExprMatch [Expr]
    deriving (Eq, Show)

