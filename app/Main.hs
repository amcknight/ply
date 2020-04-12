module Main where

import Data.ByteString.Lazy as B (ByteString, readFile)
import System.Environment (getArgs)
import CsvSql (csvPath, runQuery)
import Data.Text (pack, unpack)
import Parser (parse)
import Query (Query)

main :: IO ()
main = do
  argsStrings <- getArgs
  let input = (pack . head) argsStrings
  case parse input of
    Left errorStr -> putStr $ unpack errorStr
    Right query -> process query

process :: Query -> IO ()
process query = do
  csvData <- readTable query
  (putStr . unpack . runQuery query) csvData

readTable :: Query -> IO ByteString
readTable = B.readFile . unpack . csvPath