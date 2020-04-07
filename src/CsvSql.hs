{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( buildQuery
  , csvPath
  , runQuery
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text as T (Text, unlines)
import Query (Query, table)
import Parser (parse)
import Tokenizer (tokenize)
import Formatter (toMsg)
import Loader (loadCsv)
import Runner (run)

buildQuery :: Text -> Query
buildQuery = parse . tokenize

csvPath :: Query -> Text
csvPath query = "test/fixtures/" <> table query <> ".csv"

runQuery :: Query -> ByteString -> Text
runQuery query csvData =
  case loadCsv csvData of
    Left err -> err
    Right rows -> (T.unlines . fmap toMsg . run query) rows
