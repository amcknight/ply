{-# LANGUAGE OverloadedStrings #-}

module Element
  ( Elem(..)
  , TCol(..)
  , toElem
  , colType
  ) where

import Data.Text (Text, unpack)
import Data.Text.Read (decimal)

data Elem = SElem Text | IElem Int | BElem Bool deriving Eq
instance Show Elem where
  show (SElem s) = unpack s
  show (IElem i) = show i
  show (BElem b) = show b


data TCol = SCol | ICol | BCol deriving Eq
instance Show TCol where
  show SCol = "String"
  show ICol = "Integer"
  show BCol = "Boolean"

colType :: Elem -> TCol
colType (SElem _) = SCol
colType (IElem _) = ICol
colType (BElem _) = BCol

toElem :: Text -> Elem
toElem "True" = BElem True
toElem "False" = BElem False
toElem s =
  case decimal s of
    Right (i, _) -> IElem i
    Left _ -> SElem s
