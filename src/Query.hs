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

import Expression (Ex(..), evalEx)
import Data.Text (Text, pack)
import Data.Map.Ordered (OMap)
import Element (Elem(..))
import Table (Row)
import Utils (omap)
import QueryException
import Control.Exception.Base (throw)

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
    RowEx r -> omap (toElem csvRow) r

-- TODO: toElem should == flip, because evalEx should return an Elem
toElem :: Row -> Ex -> Elem
toElem csvRow ex =
  case evalEx ex csvRow of
    Just (LitB v) -> BElem v
    Just (LitI v) -> IElem v
    Just (LitS v) -> SElem v
    Just _ -> throw $ TypeCheckException $ pack $ "Evaluation Error: " ++ show ex
    Nothing -> throw $ TypeCheckException $ pack $ "Failed to Evaluate SELECT expression: " ++ show ex

table :: Query -> Text
table (Query _ (From t) _) = t

condition :: Query -> Maybe Ex
condition (Query _ _ (Just (Where ex))) = Just ex
condition (Query _ _ Nothing) = Nothing
