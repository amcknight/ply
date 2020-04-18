{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( parseAndProcess
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, unlines, takeEnd, unpack, pack)
import Query (Query, table, condition, selection, Selection(..))
import Expression (checkEx, ExError, Ex(LitB))
import Loader (loadCsv)
import Runner (run)
import Parser (parse)
import Element (TCol(BCol))
import Table (Table, Row, tableType)
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

checkAndProcess :: Query -> Table -> Text
checkAndProcess query = checkWhereExAndProcess query ex
  where ex = fromMaybe (LitB True) (condition query)

checkWhereExAndProcess :: Query -> Ex -> Table -> Text
checkWhereExAndProcess query ex tab =
  case typeCheck ex tab of
    Left err -> (pack . show) err
    Right BCol -> checkSelectExAndProcess query tab
    Right t -> typeCheckException t

checkSelectExAndProcess :: Query -> Table -> Text
checkSelectExAndProcess query tab =
  case selection query of
    All -> process query tab
    RowEx r -> case traverse (checkSelCol tab) r of
      Left err -> (pack . show) err
      Right _ -> process query tab

checkSelCol :: Table -> Ex -> Either ExError TCol
checkSelCol tab ex = typeCheck ex tab

typeCheck :: Ex -> Table -> Either ExError TCol
typeCheck ex = checkEx ex . tableType

typeCheckException :: TCol -> Text
typeCheckException t =
  pack . show $ TypeCheckException $ pack $ "WHERE clause evaluated to " ++ show t ++ " instead of Bool"

process :: Query -> Table -> Text
process query = pack . show . run query
