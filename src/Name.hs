{-# LANGUAGE OverloadedStrings #-}

module Name
  ( Name(..)
  , pName
  , asName
  ) where

import Data.Text (Text, pack, unpack)
import Parser (Parser, lex0, lex1)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, alphaNumChar, string')

data Name = Name
  { space :: Maybe Text
  , name :: Text
  } deriving (Eq, Ord)

instance Show Name where
  show (Name Nothing  n) = unpack n
  show (Name (Just s) n) = unpack s ++ "." ++ unpack n

pName :: Parser Name
pName = Name Nothing . pack <$> lex0 (some (alphaNumChar <|> char '_'))

asName :: Parser Name
asName = lex1 (string' "AS") *> pName
