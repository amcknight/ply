{-# LANGUAGE OverloadedStrings #-}

module Query.From
  ( From(..)
  , TableName
  , pFrom
  ) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)
import Data.Text (Text, pack)
import Parser (Parser, lex1)

type TableName = Text
newtype From = From Text deriving (Show, Eq)

pFrom :: Parser From
pFrom = From <$> (lex1 (string' "FROM") *> tableName)

tableName :: Parser TableName
tableName = pack <$> lex1 (some (alphaNumChar <|> char '_' <|> char '-' <|> char '/'))
