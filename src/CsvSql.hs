{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( go
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, unlines, takeEnd, unpack, pack)
import Query (Query, table, condition)
import Expression (checkEx)
import Loader (loadCsv)
import Runner (run)
import Formatter (toMsg)
import Parser (parse)
import Element (Row, TCol(BCol), tableType)
import QueryException (QueryException(TypeCheckException))

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
    Right rows -> checkAndRun query rows

checkAndRun :: Query -> [Row] -> Text
checkAndRun query rows =
  case condition query of
    Nothing -> actuallyRun query rows
    Just ex ->
      case checkEx ex (tableType (head rows)) of
        Left err -> pack $ show err
        Right BCol -> actuallyRun query rows
        Right t -> pack $ show $ TypeCheckException $ pack ("WHERE clause evaluated to " ++ show t ++ " instead of Bool")

actuallyRun :: Query -> [Row] -> Text
actuallyRun query = T.unlines . fmap toMsg . run query
