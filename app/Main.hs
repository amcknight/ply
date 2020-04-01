module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V
import Lib
import QueryBuilder

main :: IO ()
main = do
-- TODO: Read query from CLI
-- Parse the query into tokens or directly to an SQL query representation or emit errors if ill-formatted
  let q = build "SELECT first_name, age FROM people WHERE age = 35"
-- Validate the structure or emit errors if ill-formatted
--  from "people" $ where [EqClause "age", 35] $ select [As "first_name" "name", "age"]
-- Read the file row by row, emitting answer rows
  csvData <- BL.readFile $ "test/rsrc/" ++ "people_no_header" ++ ".csv"
  putStrLn $ loadCsv csvData

loadCsv :: BL.ByteString -> String
loadCsv csvData = case decode NoHeader csvData of
  Left err -> err
  Right v -> rowsToMessage $ V.toList v

select :: [String] -> String -> [String]
select = undefined
