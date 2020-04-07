module Main where

import Data.ByteString.Lazy as BL (readFile)
import System.Environment (getArgs)
import CsvSql (buildQuery, csvPath, runQuery)
import Data.Text as T (pack, unpack, unlines)

main :: IO ()
main = do
  argsStrings <- getArgs
  let query = (buildQuery . pack . head) argsStrings
  csvData <- (BL.readFile . unpack . csvPath) query
  (putStr . unpack . T.unlines . runQuery query) csvData
