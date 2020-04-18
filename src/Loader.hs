module Loader
  ( loadCsv
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, toList)
import Data.Map.Ordered (fromList)
import Data.Csv (decode, HasHeader(..))
import Data.Text (Text, pack, strip)
import Element (toElem)
import Table (Table(..), Row, empty, tableType, buildTable)

loadCsv :: ByteString -> Either Text Table
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left $ pack err
    Right recs -> Right $ extract $ deepToList recs

extract :: [[Text]] -> Table
extract [] = empty
extract (colNames:recs) = extractRows (fmap strip colNames) recs

extractRows :: [Text] -> [[Text]] -> Table
extractRows header vals = buildTable rows
  where rows = fmap (extractRow header) vals

-- Csv Column Names -> Csv Row values -> Output Row
extractRow :: [Text] -> [Text] -> Row
extractRow header = fromList . zip header . fmap (toElem . strip)

deepToList :: Vector (Vector a) -> [[a]]
deepToList = toList . fmap toList
