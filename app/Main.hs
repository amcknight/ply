module Main where

import qualified Data.ByteString.Lazy as BL (readFile)
import System.Environment (getArgs)
import QueryBuilder (build)
import Loader (loadCsv)
import Formatter (toMsgs)
import Query (table)

main :: IO ()
main = do
  argsStrings <- getArgs
  let queryString = head argsStrings
  let query = build queryString
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"

  let output = case loadCsv csvData of Left err -> err
                                       Right row -> toMsgs row

  putStrLn queryString
  putStrLn "----"
  putStrLn output
