{-# LANGUAGE OverloadedStrings #-}

module CsvSql
  ( parseAndProcess
  ) where

import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text as T (Text, takeEnd, unpack, pack)
import Query.Query
import Query.Select
import Query.From
import Query.Where
import Expression.Expr
import Expression.Check
import Runner (run)
import Element.Elem (ElemT(BElemT))
import Table.Table (Table, types)
import Table.Loader
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
  where From (TableName fromPath _) = from query
        csvExtension = ".csv"

loadCsvAndProcess :: Query -> ByteString -> Text
loadCsvAndProcess query csvData =
  case loadCsv csvData of
    Left err -> err
    Right rows -> checkAndProcess query rows

checkAndProcess :: Query -> Table -> Text
checkAndProcess query = checkWhereExAndProcess query ex
  where (Where ex) = fromMaybe (Where (LitB True)) (mWhere query)

checkWhereExAndProcess :: Query -> Ex -> Table -> Text
checkWhereExAndProcess query ex tab =
  case typeCheck ex tab of
    Left err -> (pack . show) err
    Right BElemT -> checkSelectExAndProcess query tab
    Right t -> pack $ "WHERE clause evaluated to " ++ show t ++ " instead of Bool"

checkSelectExAndProcess :: Query -> Table -> Text
checkSelectExAndProcess query tab =
  case select query of
    Select All -> process query tab
    Select (RowEx r) -> case traverse (checkSelCol tab) r of
      Left err -> (pack . show) err
      Right _ -> process query tab

checkSelCol :: Table -> Ex -> Either ExError ElemT
checkSelCol tab ex = typeCheck ex tab

typeCheck :: Ex -> Table -> Either ExError ElemT
typeCheck ex = checkEx ex . types

process :: Query -> Table -> Text
process query = pack . show . run query
