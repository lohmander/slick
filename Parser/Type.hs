module Parser.Type
  ( pType
  , pListType
  , pPrimType
  ) where


import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Expression
import           Parser.Lexer


pType :: Parser Type
pType = try pFuncType
    <|> try pListType
    <|> try pTupleType
    <|> try pPrimType


pListType :: Parser Type
pListType = do
    type' <- brackets pType
    return $ TypeList type'


pTupleType :: Parser Type
pTupleType = do
    types <- parens $ P.sepBy pType comma
    return $ TypeTuple types


pFuncType :: Parser Type
pFuncType = do
    type' <- parens p
    return type'
  where
    p = L.lineFold scn $ \sc' -> do
      params <- P.sepBy pType comma
      sc'
      arrow
      sc'
      returns <- pType
      return $ TypeFunc params returns

pPrimType :: Parser Type
pPrimType = do
    typeStr <- ident
    return $
      case typeStr of
          "String" -> TypeList TypeChar
          "Int"    -> TypeInt
          "Float"  -> TypeFloat
          "Bool"   -> TypeBool
          "Char"   -> TypeChar
          x        -> TypeGeneric x
