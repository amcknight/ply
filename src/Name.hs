module Name
  ( Name
  , pName
  ) where

import Data.Text (Text, pack)
import Parser (Parser, lex0)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, alphaNumChar)

type Name = Text

pName :: Parser Name
pName = pack <$> lex0 (some (alphaNumChar <|> char '_'))
