module Table.Row
  ( Row
  , RowT
  , toMsg
  , Table.Row.getType
  , empty
  , emptyT
  ) where

import Data.Text as T (Text)
import qualified Data.Map.Ordered as O (OMap, empty)
import Element.Elem as E
import Data.List (intercalate)
import Table.Utils (omap, ovals)

type Row = O.OMap Text Elem
type RowT = O.OMap Text ElemT

getType :: Row -> RowT
getType = omap E.getType

toMsg :: Row -> String
toMsg = intercalate ", " . fmap show . ovals

empty :: Row
empty = O.empty

emptyT :: RowT
emptyT = O.empty
