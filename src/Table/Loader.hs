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

loadCsv :: Text -> ByteString -> Either Text Table
loadCsv tName csvData =
  case decode NoHeader csvData of
    Left err -> (Left . pack) err
    Right ls -> (Right . extract tName . toRecs) ls

extract :: Text -> [Rec] -> Table
extract _ [] = empty
extract tName (header:recs) = extractRows tName (fmap strip header) recs

extractRows :: Text -> Rec -> [Rec] -> Table
extractRows tName header vals = buildTable rs
  where rs = fmap (extractRow tName header) vals

-- Csv Header -> Csv Value -> Output Row
extractRow :: Text -> Rec -> Rec -> Row
extractRow tName header = fromList . zip (Name tName <$> header) . fmap (toElem . strip)
