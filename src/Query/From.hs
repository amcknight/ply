module Query.From
  ( From(..)
  , TableName(..)
  , pFrom
  ) where

import Name
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, alphaNumChar)
import Data.Text (Text, pack, unpack)
import Parser (Parser, lex1)
import System.FilePath (takeBaseName)

data TableName = TableName
  { path :: Text
  , name :: Text
  } deriving (Show, Eq)
newtype From = From TableName deriving (Show, Eq)

pFrom :: Parser From
pFrom = From <$> tableName

tableName :: Parser TableName
tableName = do
  p <- pPath
  a <- try asNameText <|> (pure . pack . takeBaseName . unpack) p
  return $ TableName p a

pPath :: Parser Text
pPath = pack <$> lex1 (some (alphaNumChar <|> char '_' <|> char '-' <|> char '/'))
