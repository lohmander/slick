module Parser.Expression
  ( pExpr
  , pVar
  , pCase
  ) where

import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Lexer


pExpr :: Parser Expr
pExpr = try pVar
    <|> try pInt
    <|> try pString
    <|> try pChar
    <|> try pDiscard


pVar :: Parser Expr
pVar = do
    var <- ident
    return $ ExprVar var


pInt :: Parser Expr
pInt = do
    int <- integer
    return $ ExprLit $ LitInt int


pDiscard :: Parser Expr
pDiscard = do
    underscore
    return ExprDiscard


pString :: Parser Expr
pString = do
    str <- P.between quote quote string
    return $ ExprLit $ LitString str


pChar :: Parser Expr
pChar = do
    chr <- P.between squote squote char
    return $ ExprLit $ LitChar chr


pCase :: Parser Expr
pCase = L.lineFold scn $ \sc' -> do
    pipe
    params <- P.sepBy pExpr comma
    arrow
    sc'
    expr <- pExpr
    sc
    return $ ExprCase (ExprMatch params) expr


