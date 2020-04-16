module Main where

import System.Environment (getArgs)
import CsvSql (parseAndProcess)
import Data.Text (Text, pack, unpack)

main :: IO ()
main = do
  query <- optParse
  res <- parseAndProcess query
  putStr $ unpack res

optParse :: IO Text
optParse = pack . head <$> getArgs
