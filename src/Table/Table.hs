module Table.Table
  ( Table(..)
  , RowT
  , buildTable
  , Table.Table.empty
  ) where

import Table.Row as R
import Data.Text as T (unpack)
import Table.Utils (okeys)
import Data.List (intercalate)

data Table = Table
  { types :: RowT
  , rows :: [Row]
  }

instance Show Table where
  show tab = unlines (header:rs)
    where header = (intercalate ", " . fmap unpack . okeys) (types tab)
          rs = fmap toMsg (rows tab)

buildTable :: [Row] -> Table
buildTable rs = Table (commonType rs) rs

-- TODO: This should make sure all rows have the same type
commonType :: [Row] -> RowT
commonType rs = R.getType $ head rs

empty :: Table
empty = Table emptyT []
