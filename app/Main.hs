{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL (readFile)
import System.Environment (getArgs)
import Tokenizer (tokenize)
import Parser (parse)
import Loader (loadCsv)
import Formatter (toMsgs)
import Runner (run)
import Query (table)
import Data.Text (pack, unpack)

main :: IO ()
main = do
  argsStrings <- getArgs
  let query = parse . tokenize $ pack $ head argsStrings
  csvData <- BL.readFile $ unpack $ "test/fixtures/" <> table query <> ".csv"

  let output = case loadCsv csvData of Left err -> err
                                       Right rows -> toMsgs $ run query rows

  print query
  putStrLn "----"
  putStrLn $ unpack output
