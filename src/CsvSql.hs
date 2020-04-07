{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( buildQuery
  , csvPath
  , runQuery
  ) where

import Data.ByteString.Lazy (ByteString)
import Query (Query, table)
import Data.Text (Text)
import Parser (parse)
import Tokenizer (tokenize)
import Formatter (toMsgs)
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
    Right rows -> toMsgs $ run query rows