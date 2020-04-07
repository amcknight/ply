{-# LANGUAGE OverloadedStrings #-}

module Formatter
  ( toMsg
  ) where

import Element (Row, Elem(..))
import Data.Text as T (Text, pack, intercalate)
import Data.Map (elems)

toMsg :: Row -> Text
toMsg = intercalate " | " . fmap convert . elems

convert :: Elem -> Text
convert (SElem s) = s
convert (IElem i) = pack (show i)
