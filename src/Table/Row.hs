module Table.Row
  ( Row
  , RowT
  , buildRow
  , toMsg
  , Table.Row.getType
  , empty
  , emptyT
  ) where

import qualified Data.Map.Ordered as O (OMap, empty, fromList)
import Element.Elem as E
import Table.Utils (omap, ovals)
import Name (Name)
import Data.List (intercalate)

type Row = O.OMap Name Elem
type RowT = O.OMap Name ElemT

buildRow :: [Name] -> [Elem] -> Row
buildRow header elems = O.fromList $ zip header elems

getType :: Row -> RowT
getType = omap E.getType

toMsg :: Row -> String
toMsg = intercalate ", " . fmap show . ovals

empty :: Row
empty = O.empty

emptyT :: RowT
emptyT = O.empty
