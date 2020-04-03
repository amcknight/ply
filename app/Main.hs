module Main where

import qualified Data.ByteString.Lazy as BL (readFile)
import System.Environment (getArgs)
import Builder (build)
import Loader (loadCsv)
import Formatter (toMsgs)
import Runner (run)
import Query (table)

main :: IO ()
main = do
  argsStrings <- getArgs
  let queryString = head argsStrings
  let query = build queryString
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"

  let output = case loadCsv csvData of Left err -> err
                                       Right rows -> toMsgs $ run query rows

  putStrLn queryString
  putStrLn "----"
  putStrLn output
