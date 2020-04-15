module Main where

import System.Environment (getArgs)
import CsvSql (go)
import Data.Text (Text, pack, unpack)

main :: IO ()
main = do
  query <- optParse
  res <- go query
  putStr $ unpack res

optParse :: IO Text
optParse = pack . head <$> getArgs
