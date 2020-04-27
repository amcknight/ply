module Parser
  ( Parser
  , ParserError
  , lex0
  , lex1
  ) where

import Data.Void
import Data.Text (Text)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

lex0 :: Parser a -> Parser a
lex0 = L.lexeme space

lex1 :: Parser a -> Parser a
lex1 = L.lexeme space1
