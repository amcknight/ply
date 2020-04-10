{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( buildQuery
  , csvPath
  , runQuery
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text as T (Text, unlines, takeEnd)
import Query (Query, table)
import Parser (parse)
import Tokenizer (tokenize)
import Loader (loadCsv)
import Runner (run)
import Formatter (toMsg)

buildQuery :: Text -> Query
buildQuery = parse . tokenize

csvPath :: Query -> Text
csvPath query =
  if takeEnd 4 fromPath == ".csv"
    then fromPath
    else fromPath <> ".csv"
  where fromPath = table query

runQuery :: Query -> ByteString -> Text
runQuery query csvData =
  case loadCsv csvData of
    Left err -> err
    Right rows -> (T.unlines . fmap toMsg . run query) rows
