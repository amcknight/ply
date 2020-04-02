module Formatter
  ( toMsgs
  ) where

type Row = (String, String, Int)

toMsgs :: [Row] -> String
toMsgs rows = unlines $ fmap toMsg rows

toMsg :: Row -> String
toMsg (first_name, _, age) = first_name ++ ", " ++ show age
