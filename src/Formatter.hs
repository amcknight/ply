module Formatter
  ( toMsgs
  ) where

import Data.List (intercalate)

toMsgs :: [[String]] -> String
toMsgs elems = unlines $ fmap toMsg elems

toMsg :: [String] -> String
toMsg = intercalate ", "
