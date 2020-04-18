module Query.Select
  ( Select(..)
  , Selection(..)
  , evalSel
  ) where

import Name
import Element.Elem
import Expression.Expr
import Data.Map.Ordered (OMap)
import Table (Row)
import Utils (omap)
import Expression.Eval (evalEx)

-- Select
data Selection = All | RowEx (OMap Name Ex) deriving (Show, Eq)
newtype Select = Select Selection deriving (Show, Eq)

evalSel :: Select -> Row -> Row
evalSel (Select All) csvRow = csvRow
evalSel (Select (RowEx rs)) csvRow = omap (evalRow csvRow) rs

evalRow :: Row -> Ex -> Elem
evalRow csvRow ex = evalEx ex csvRow