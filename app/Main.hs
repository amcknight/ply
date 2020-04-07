module Main where

import Data.ByteString.Lazy as BL (ByteString, readFile)
import System.Environment (getArgs)
import CsvSql (buildQuery, csvPath, runQuery)
import Query (Query)
import Data.Text (Text, pack, unpack)

main :: IO ()
main = do
  argsStrings <- getArgs
  let query = buildQuery $ queryTextFromOpts argsStrings
  csvData <- csv query
  print $ runQuery query csvData

queryTextFromOpts :: [String] -> Text
queryTextFromOpts = pack . head

csv :: Query -> IO ByteString
csv = BL.readFile . unpack . csvPath