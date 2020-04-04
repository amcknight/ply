module Formatter
  ( toMsgs
  ) where

import Element as E
import qualified Data.Map as M (foldr)

toMsgs :: [E.Row] -> String
toMsgs elems = unlines $ fmap toMsg elems

toMsg :: E.Row -> String
toMsg = M.foldr convert ""

convert :: Elem -> String -> String
convert (E.SElem s) = ((s ++ ", ") ++)
convert (E.IElem i) = ((show i ++ ", ") ++)
