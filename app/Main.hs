module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V
import Lib

main :: IO ()
--main = select ("first_name", "age")
main = do
  csvData <- BL.readFile "test/rsrc/people_no_header.csv"
  putStrLn $ loadCsv csvData

loadCsv :: BL.ByteString -> String
loadCsv csvData = case decode NoHeader csvData of
  Left err -> err
  Right v -> rowsToMessage $ V.toList v

select :: [String] -> String -> [String]
select = undefined
