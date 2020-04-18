module Element
  ( Elem(..)
  , Row
  , TRow
  , TCol(..)
  , empty
  , tableType
  ) where

import Data.Text (Text)
import Data.Map.Ordered as O (OMap, empty)
import Utils (omap)

data Elem = SElem Text | IElem Int | BElem Bool deriving (Eq, Show)

type Row = OMap Text Elem

data TCol = SCol | ICol | BCol deriving Eq
instance Show TCol where
  show SCol = "String"
  show ICol = "Integer"
  show BCol = "Boolean"

type TRow = OMap Text TCol

tableType :: Row -> TRow
tableType = omap colType

colType :: Elem -> TCol
colType (SElem _) = SCol
colType (IElem _) = ICol
colType (BElem _) = BCol
