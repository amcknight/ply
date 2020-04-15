{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( go
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, unlines, takeEnd, unpack)
import Query (Query, table)
import Loader (loadCsv)
import Runner (run)
import Formatter (toMsg)
import Parser (parse)

go :: Text -> IO Text
go input =
  case parse input of
    Left errorStr -> pure errorStr
    Right query -> process query

process :: Query -> IO Text
process query = do
  csvData <- (B.readFile . unpack . csvPath) query
  pure $ runQuery query csvData

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
