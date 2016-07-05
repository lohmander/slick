module Parser.Lexer
    ( scn
    , sc
    , rWord
    , lexeme
    , ident
    , parens
    , braces
    , brackets
    , string
    , char
    , integer
    , symbol
    , plus
    , minus
    , times
    , slash
    , equal
    , comma
    , dot
    , arrow
    , equality
    , colon
    , pipe
    , underscore
    , quote
    , squote
    ) where


import           Control.Applicative    (empty)
import           Control.Monad          (void)

import           Text.Megaparsec        ((<|>))
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String



reservedWords :: [String]
reservedWords =
    [ "from"
    , "import"
    , "module"

    , "and"
    , "or"
    , "not"
    ]


lineComment :: Parser ()
lineComment = L.skipLineComment "#"


scn :: Parser ()
scn = L.space (void P.spaceChar) lineComment empty


sc :: Parser ()
sc = L.space (void $ P.oneOf " \t") lineComment empty


rWord :: String -> Parser ()
rWord w = P.string w *> P.notFollowedBy P.alphaNumChar *> sc


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


ident :: Parser String
ident = lexeme (p >>= check)
  where
    p = (:) <$> P.letterChar <*> P.many (P.alphaNumChar <|> P.char '_')
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " is reserved"
                 else return x


parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")


braces :: Parser a -> Parser a
braces = P.between (symbol"{") (symbol "}")


brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")



-- LITERALS


string :: Parser String
string = (:) <$> P.letterChar <*> P.many (P.noneOf "\"" <|> P.spaceChar)


char :: Parser Char
char = P.anyChar


integer :: Parser Integer
integer = lexeme L.integer



-- SYMBOLS


symbol :: String -> Parser String
symbol = L.symbol sc


plus :: Parser String
plus = symbol "+"


minus :: Parser String
minus = symbol "-"


times :: Parser String
times = symbol "*"


slash :: Parser String
slash = symbol "/"


equal :: Parser String
equal = symbol "="


comma :: Parser String
comma = symbol ","


dot :: Parser String
dot = symbol "."


arrow :: Parser String
arrow = symbol "->"


equality :: Parser String
equality = symbol "=="


colon :: Parser String
colon = symbol ":"


pipe :: Parser String
pipe = symbol "|"


underscore :: Parser String
underscore = symbol "_"


quote :: Parser String
quote = symbol "\""


squote :: Parser String
squote = symbol "'"
