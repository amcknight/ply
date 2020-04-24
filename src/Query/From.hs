{-# LANGUAGE OverloadedStrings #-}

module Query.From
  ( From(..)
  , TableName(..)
  , pFrom
  ) where

import Name
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)
import Data.Text (Text, pack, unpack)
import Parser (Parser, lex1)
import System.FilePath (takeBaseName)

data TableName = TableName
  { path :: Text
  , name :: Name
  } deriving (Show, Eq)
newtype From = From TableName deriving (Show, Eq)

pFrom :: Parser From
pFrom = From <$> (lex1 (string' "FROM") *> tableName)

tableName :: Parser TableName
tableName = do
  p <- pPath
  a <- try asName <|> pure (pathToName p)
  return $ TableName p a

pPath :: Parser Text
pPath = pack <$> lex1 (some (alphaNumChar <|> char '_' <|> char '-' <|> char '/'))

pathToName :: Text -> Name
pathToName = Name Nothing . pack . takeBaseName . unpack
