module Table
  ( Row
  , TRow
  , Table(..)
  , buildTable
  , empty
  , extractTRow
  ) where

import Data.Text as T (Text, unpack)
import qualified Data.Map.Ordered as O (OMap, empty)
import Element.Elem
import Utils (omap, okeys, ovals)
import Data.List (intercalate)

data Table = Table
  { types :: TRow
  , rows :: [Row]
  }

instance Show Table where
  show tab = unlines (header:rs)
    where header = (intercalate ", " . fmap unpack . okeys) (types tab)
          rs = fmap toMsg (rows tab)

type TRow = O.OMap Text ElemT
type Row = O.OMap Text Elem

toMsg :: Row -> String
toMsg = intercalate ", " . fmap show . ovals

buildTable :: [Row] -> Table
buildTable rs = Table (extractTRow rs) rs

extractTRow :: [Row] -> TRow
extractTRow rs = omap getType (head rs)

empty :: Table
empty = Table O.empty []
