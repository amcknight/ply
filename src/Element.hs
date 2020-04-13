module Element
  ( Elem(..)
  , Row
  ) where

import Data.Text (Text)
import Data.Map.Ordered (OMap)

data Elem = SElem Text | IElem Int | BElem Bool deriving (Eq, Show)

type Row = OMap Text Elem
