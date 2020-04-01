module Main where

import qualified Data.ByteString.Lazy as BL
import QueryBuilder
import Loader
import Printer

main :: IO ()
main = do
  let queryString = "\
    \SELECT first_name, age\n\
    \FROM people\n\
    \WHERE age = 35"
  putStrLn queryString
  putStrLn "----"
  let query = build queryString
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"
  putStrLn $ case loadCsv csvData of Left err -> err
                                     Right row -> rowsToMessage row
