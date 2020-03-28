module Lib
  ( rowsToMessage
  ) where

type Row = (String, String, Int)

rowsToMessage :: [(String, String, Int)] -> String
rowsToMessage rows = unlines $ fmap rowToMessage rows

rowToMessage :: (String, String, Int) -> String
rowToMessage (first_name, _, age) = first_name ++ " is " ++ show age ++ " years old"
