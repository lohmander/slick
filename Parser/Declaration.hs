module Parser.Declaration where


import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Expression
import           Parser.Lexer
import           Parser.Type


pDecl :: Parser Decl
pDecl = try pFunc


pFunc :: Parser Decl
pFunc = L.indentBlock scn p
  where
    p = do
        name <- ident
        sign <- pFuncSign
        return (L.IndentMany Nothing (return . \c -> DeclFunc name sign c) pCase)


pFuncSign :: Parser Decl
pFuncSign = L.lineFold scn $ \sc' -> do
    colon
    params <- P.sepBy pTypeDecl comma
    sc'
    arrow
    sc'
    rtrn <- pTypeDecl
    return $ DeclSign params rtrn


pTypeDecl :: Parser Decl
pTypeDecl = do
    type' <- pType
    return $ DeclType type'



