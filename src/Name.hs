{-# LANGUAGE OverloadedStrings #-}

module Name
  ( Name(..)
  , pName
  , asName
  , asNameText
  ) where

import Data.Text (Text, pack, unpack)
import Parser (Parser, lex0, lex1)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, alphaNumChar, string')

data Name = Name
  { space :: Text
  , name :: Text
  } deriving (Eq, Ord)

instance Show Name where
  show (Name s n) = unpack s ++ "." ++ unpack n

pName :: Text -> Parser Name
pName tName = try pNameSpace <|> pNameNoSpace tName

pNameSpace :: Parser Name
pNameSpace = do
  p1 <- pText
  p2 <- char '.' *> pText
  return $ Name p1 p2

pNameNoSpace :: Text -> Parser Name
pNameNoSpace tName = Name tName <$> pText

pText :: Parser Text
pText = pack <$> lex0 (some (alphaNumChar <|> char '_'))

asName :: Text -> Parser Name
asName tName = Name tName <$> asNameText

asNameText :: Parser Text
asNameText = lex1 (string' "AS") *> pText