{-# LANGUAGE OverloadedStrings #-}

module Formatter
  ( toMsgs
  ) where

import Element as E
import qualified Data.Text as T
import qualified Data.Map as M (elems)

toMsgs :: [E.Row] -> T.Text
toMsgs = T.unlines . fmap toMsg

toMsg :: E.Row -> T.Text
toMsg row = T.intercalate " | " (fmap convert (M.elems row))

convert :: Elem -> T.Text
convert (E.SElem s) = s
convert (E.IElem i) = T.pack (show i)
