module Parser.Expression
  ( pExpr
  , pVar
  , pCase
  ) where

import           Data.List
import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Lexer


pExpr :: Parser Expr
pExpr = try pParens
    <|> try aExpr
    <|> try pTerm
    <|> try pDiscard


pTerm :: Parser Expr
pTerm = try pVar
    <|> try pInt
    <|> try pString
    <|> try pChar


aOp :: [[Operator Parser Expr]]
aOp =
    [ [ InfixL (times *> pure (AOp OpMultiply))
      , InfixL (slash *> pure (AOp OpDivide))
      ]
    , [ InfixL (plus *> pure (AOp OpPlus))
      , InfixL (minus *> pure (AOp OpMinus))
      ]
    ]


aExpr :: Parser Expr
aExpr = makeExprParser pTerm aOp


pVar :: Parser Expr
pVar = do
    var <- ident
    return $ ExprVar var


pInt :: Parser Expr
pInt = do
    int <- integer
    return $ ExprLit $ LitInt int


pString :: Parser Expr
pString = do
    str <- P.between quote quote string
    return $ ExprLit $ LitList $ map (\c -> ExprLit $ LitChar c) str


pChar :: Parser Expr
pChar = do
    chr <- P.between squote squote char
    return $ ExprLit $ LitChar chr


pParens :: Parser Expr
pParens = do
    expr <- parens pExpr
    return $ ExprParens expr


pDiscard :: Parser Expr
pDiscard = do
    underscore
    return ExprDiscard



pCase :: Parser Expr
pCase = L.lineFold scn $ \sc' -> do
    pipe
    params <- P.sepBy pExpr comma
    arrow
    sc'
    expr <- pExpr
    sc
    return $ ExprCase (ExprMatch params) expr


