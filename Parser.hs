module Parser (parser) where

import           AST
import           Parser.Declaration
import           Parser.Lexer

import qualified Text.Megaparsec        as P
import           Text.Megaparsec.String


parser :: Parser [Decl]
parser = (P.some pDecl) <* P.eof

