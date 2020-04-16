module Element
  ( Elem(..)
  , Row
  , TRow
  , TCol(..)
  , empty
  , tableType
  ) where

import Data.Text (Text)
import Data.Map.Ordered as O (OMap, empty, assocs, fromList)

data Elem = SElem Text | IElem Int | BElem Bool deriving (Eq, Show)

type Row = OMap Text Elem

data TCol = SCol | ICol | BCol deriving Eq
instance Show TCol where
  show SCol = "String"
  show ICol = "Integer"
  show BCol = "Boolean"

type TRow = OMap Text TCol

tableType :: Row -> TRow
tableType row = O.fromList $ fmap (\(k, v) -> (k, colType v)) (O.assocs row)

colType :: Elem -> TCol
colType (SElem _) = SCol
colType (IElem _) = ICol
colType (BElem _) = BCol
