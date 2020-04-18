module Table
  ( Row
  , TRow
  , Table(..)
  , buildTable
  , empty
  , tableType
  , rows
  , extractTRow
  ) where

import Data.Text as T (Text, unpack)
import qualified Data.Map.Ordered as O (OMap, empty, assocs)
import Element (Elem(..), TCol(..), colType)
import Utils (omap)
import Data.List (intercalate)

data Table = Table TRow [Row]

instance Show Table where
  show tab = unlines (header:rs)
    where header = (intercalate ", " . fmap (unpack . fst) . O.assocs) (tableType tab)
          rs = fmap toMsg (rows tab)

toMsg :: Row -> String
toMsg = intercalate ", " . fmap (show . snd) . O.assocs

rows :: Table -> [Row]
rows (Table _ rs) = rs

type TRow = O.OMap Text TCol
type Row = O.OMap Text Elem

tableType :: Table -> TRow
tableType (Table t _) = t

buildTable :: [Row] -> Table
buildTable rs = Table (extractTRow rs) rs

extractTRow :: [Row] -> TRow
extractTRow rs = omap colType (head rs)

empty :: Table
empty = Table O.empty []
