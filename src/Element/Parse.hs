{-# LANGUAGE OverloadedStrings #-}

module Element.Parse 
  ( toElem
  ) where

import Element.Elem
import Data.Text
import Data.Text.Read (decimal)

toElem :: Text -> Elem
toElem "True" = BElem True
toElem "False" = BElem False
toElem s =
  case decimal s of
    Right (i, _) -> IElem i
    Left _ -> SElem s
