module Main where

import qualified Data.ByteString.Lazy as BL
import Printer
import Loader
import QueryBuilder

main :: IO ()
main = do
-- TODO: Read query from CLI
-- Parse the query into tokens or directly to an SQL query representation or emit errors if ill-formatted
  let query = build "SELECT first_name, age FROM people WHERE age = 35"
-- Validate the structure or emit errors if ill-formatted
-- Read the file row by row, emitting answer rows
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"
  putStrLn $ case loadCsv csvData of Left err -> err
                                     Right row -> rowsToMessage row
