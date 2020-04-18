module Query
  ( Query(..)
  , Select(..)
  , Selection(..)
  , Col
  , From(..)
  , Table
  , Where(..)
  , selection
  , evalSel
  , table
  , condition
  ) where

import Expression.Expr
import Expression.Eval
import Data.Text (Text)
import Data.Map.Ordered (OMap)
import Element.Elem (Elem(..))
import Table (Row)
import Utils (omap)

-- Select
type Col = Text
data Selection = All | RowEx (OMap Col Ex) deriving (Show, Eq)
newtype Select = Select Selection deriving (Show, Eq)

-- From
type Table = Text
newtype From = From Table deriving (Show, Eq)

-- Where
newtype Where = Where Ex deriving (Show, Eq)

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

selection :: Query -> Selection
selection (Query (Select ss) _ _) = ss

evalSel :: Query -> Row -> Row
evalSel query csvRow =
  case selection query of
    All -> csvRow
    RowEx rs -> omap (evalRow csvRow) rs

evalRow :: Row -> Ex -> Elem
evalRow csvRow ex = evalEx ex csvRow

table :: Query -> Text
table (Query _ (From t) _) = t

condition :: Query -> Maybe Ex
condition (Query _ _ (Just (Where ex))) = Just ex
condition (Query _ _ Nothing) = Nothing
