{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( parseAndProcess
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, unlines, takeEnd, unpack, pack)
import Query (Query, table, condition, selection)
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
    Right rows -> checkAndProcess query rows

checkAndProcess :: Query -> [Row] -> Text
checkAndProcess query = checkWhereExAndProcess query ex
  where ex = fromMaybe (LitB True) (condition query)

checkWhereExAndProcess :: Query -> Ex -> [Row] -> Text
checkWhereExAndProcess query ex rows =
  case typeCheck ex (head rows) of
    Left err -> (pack . show) err
    Right BCol -> checkSelectExAndProcess query rows
    Right t -> typeCheckException t

checkSelectExAndProcess :: Query -> [Row] -> Text
checkSelectExAndProcess query rows =
  case traverse (checkSelCol (head rows)) (selection query) of
    Left err -> (pack . show) err
    Right _ -> process query rows

checkSelCol :: Row -> Ex -> Either ExError TCol
checkSelCol row ex = typeCheck ex row

typeCheck :: Ex -> Row -> Either ExError TCol
typeCheck ex = checkEx ex . tableType

typeCheckException :: TCol -> Text
typeCheckException t =
  pack . show $ TypeCheckException $ pack $ "WHERE clause evaluated to " ++ show t ++ " instead of Bool"

process :: Query -> [Row] -> Text
process query = T.unlines . fmap toMsg . run query
