module Element
  ( Elem(..)
  , Row
  , empty
  ) where

import Data.Text (Text)
import Data.Map.Ordered as O (OMap, empty)

data Elem = SElem Text | IElem Int | BElem Bool deriving (Eq, Show)

type Row = OMap Text Elem
