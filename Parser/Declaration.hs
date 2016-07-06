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
    ps = L.lineFold scn $ \sc' -> do
      colon
      params <- P.sepBy pType comma
      sc'
      arrow
      sc'
      returns <- pType
      return $ DeclSign params returns

    p = do
      name <- ident
      sign <- ps
      return (L.IndentMany Nothing (return . \c -> DeclFunc name sign c) pCase)

