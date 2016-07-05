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
pType = try pListType
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


pPrimType :: Parser Type
pPrimType = do
    typeStr <- ident
    return $
      case typeStr of
          "string" -> TypeString
          "int"    -> TypeInt
          "float"  -> TypeFloat
          "bool"   -> TypeBool
          "char"   -> TypeChar
