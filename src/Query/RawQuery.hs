{-# LANGUAGE OverloadedStrings #-}

module Query.RawQuery
  ( RawQuery(..)
  , pRawQuery
  ) where

import Parser
import Text.Megaparsec (optional, someTill, some, try, (<|>))
import Data.Text
import Text.Megaparsec.Char (string')
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral)

data RawQuery = RawQuery
  { select :: Text
  , from :: Text
  , mWhere :: Maybe Text
  } deriving (Show, Eq)

pRawQuery :: Parser RawQuery
pRawQuery = RawQuery <$> pRawSelect <*> pRawFrom <*> optional pRawWhere

pRawSelect :: Parser Text
pRawSelect = pack <$> (lex1 (string' "SELECT") *> someTill L.charLiteral (lex1 (string' "FROM")))

pRawFrom :: Parser Text
pRawFrom = pack <$> (try (someTill L.charLiteral (lex1 (string' "WHERE"))) <|> some L.charLiteral)

pRawWhere :: Parser Text
pRawWhere = pack <$> some L.charLiteral
