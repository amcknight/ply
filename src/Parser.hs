{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Query.Query
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)

parse :: Text -> Either Text Query
parse query =
  case runParser (pQuery <* eof) "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q
