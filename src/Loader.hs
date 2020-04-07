module Loader
  ( loadCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decode, HasHeader(..))
import Data.Text (Text, pack, strip)
import Data.Text.Read (decimal)
import Data.Map as M (fromList)
import Element as E

loadCsv :: BL.ByteString -> Either Text [E.Row]
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left $ pack err
    Right recs -> Right $ extract $ deepToList recs

extract :: [[Text]] -> [E.Row]
extract [] = []
extract (colNames:recs) = extractRows (fmap strip colNames) recs

-- Csv Column Names -> Csv Row values -> Output Rows
extractRows :: [Text] -> [[Text]] -> [E.Row]
extractRows = fmap . extractOne

-- Csv Column Names -> Csv Row values -> Output Row
extractOne :: [Text] -> [Text] -> E.Row
extractOne colNames = M.fromList . zip colNames . fmap (toElem . strip)

deepToList :: V.Vector (V.Vector a) -> [[a]]
deepToList = V.toList . fmap V.toList

-- Ultimately this should be a proper value parser
toElem :: Text -> Elem
toElem s =
  case decimal s of
    Left _ -> E.SElem $ pack "\"" <> s <> pack "\""
    Right (i, _) -> E.IElem i
