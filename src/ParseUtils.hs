module ParseUtils
  ( Parser
  , lex0
  , lex1
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lex0 :: Parser a -> Parser a
lex0 = L.lexeme space

lex1 :: Parser a -> Parser a
lex1 = L.lexeme space1
