module Main where

import qualified Data.ByteString.Lazy as BL (readFile)
import System.Environment (getArgs)
import Tokenizer (tokenize)
import Parser (parse)
import Loader (loadCsv)
import Formatter (toMsgs)
import Runner (run)
import Query (table)

main :: IO ()
main = do
  argsStrings <- getArgs
  let query = parse . tokenize $ head argsStrings
  csvData <- BL.readFile $ "test/rsrc/" ++ table query ++ ".csv"

  let output = case loadCsv csvData of Left err -> err
                                       Right rows -> toMsgs $ run query rows

  print query
  putStrLn "----"
  putStrLn output
