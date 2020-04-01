module Printer
  ( rowsToMessage
  ) where

type Row = (String, String, Int)

rowsToMessage :: [Row] -> String
rowsToMessage rows = unlines $ fmap rowToMessage rows

rowToMessage :: Row -> String
rowToMessage (first_name, _, age) = first_name ++ " is " ++ show age ++ " years old"
