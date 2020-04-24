module Table.Loader
  ( loadCsv
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Ordered (fromList)
import Data.Csv (decode, HasHeader(..))
import Data.Text (Text, pack, strip)
import Element.Parse
import Table.Table (Table(..), empty, buildTable)
import Table.Row (Row)
import Table.Rec
import Name

loadCsv :: ByteString -> Either Text Table
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left $ pack err
    Right ls -> Right $ extract $ toRecs ls

extract :: [Rec] -> Table
extract [] = empty
extract (header:recs) = extractRows (fmap strip header) recs

extractRows :: Rec -> [Rec] -> Table
extractRows header vals = buildTable rs
  where rs = fmap (extractRow header) vals

-- Csv Header -> Csv Value -> Output Row
extractRow :: Rec -> Rec -> Row
extractRow header = fromList . zip (Name Nothing <$> header) . fmap (toElem . strip)
