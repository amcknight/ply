module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import QueryBuilder
import Loader
import Formatter

main :: IO ()
main = do
  argsStrings <- getArgs
  let queryString = head argsStrings
  let query = build queryString
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"

  let output = case loadCsv csvData of Left err -> err
                                       Right row -> rowsToMessage row

  putStrLn queryString
  putStrLn "----"
  putStrLn output
