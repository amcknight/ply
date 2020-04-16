{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( parseAndProcess
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, unlines, takeEnd, unpack, pack)
import Query (Query, table, condition)
import Expression (checkEx, ExError, Ex(LitB))
import Loader (loadCsv)
import Runner (run)
import Formatter (toMsg)
import Parser (parse)
import Element (Row, TCol(BCol), tableType)
import QueryException (QueryException(TypeCheckException))
import Data.Maybe (fromMaybe)

parseAndProcess :: Text -> IO Text
parseAndProcess input =
  case parse input of
    Left errStr -> pure errStr
    Right query -> readAndProcess query

readAndProcess :: Query -> IO Text
readAndProcess query = do
  csvData <- (B.readFile . unpack . csvPath) query
  pure $ loadCsvAndProcess query csvData

csvPath :: Query -> Text
csvPath query =
  if takeEnd 4 fromPath == csvExtension
    then fromPath
    else fromPath <> csvExtension
  where fromPath = table query
        csvExtension = ".csv"

loadCsvAndProcess :: Query -> ByteString -> Text
loadCsvAndProcess query csvData =
  case loadCsv csvData of
    Left err -> err
    Right rows -> checkWhereAndProcess query rows

checkWhereAndProcess :: Query -> [Row] -> Text
checkWhereAndProcess query = checkExAndProcess query ex
  where ex = fromMaybe (LitB True) (condition query)

checkExAndProcess :: Query -> Ex -> [Row] -> Text
checkExAndProcess query ex rows =
  case typeCheck ex rows of
    Left err -> (pack . show) err
    Right BCol -> process query rows
    Right t -> typeCheckException t

typeCheck :: Ex -> [Row] -> Either ExError TCol
typeCheck ex = checkEx ex . tableType . head

typeCheckException :: TCol -> Text
typeCheckException t =
  pack . show $ TypeCheckException $ pack $ "WHERE clause evaluated to " ++ show t ++ " instead of Bool"

process :: Query -> [Row] -> Text
process query = T.unlines . fmap toMsg . run query
