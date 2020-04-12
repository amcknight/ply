{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( csvPath
  , runQuery
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text as T (Text, unlines, takeEnd)
import Query (Query, table)
import Loader (loadCsv)
import Runner (run)
import Formatter (toMsg)

csvPath :: Query -> Text
csvPath query =
  if takeEnd 4 fromPath == csvExtension
    then fromPath
    else fromPath <> csvExtension
  where fromPath = table query
        csvExtension = ".csv"

runQuery :: Query -> ByteString -> Text
runQuery query csvData =
  case loadCsv csvData of
    Left err -> err
    Right rows -> (T.unlines . fmap toMsg . run query) rows
