module Main where

import System.Environment (getArgs)
import CsvSql (go)
import Data.Text (pack, unpack)

main :: IO ()
main = do
  argsStrings <- getArgs
  res <- (go . pack . head) argsStrings
  (putStr . unpack) res
