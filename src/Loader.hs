{-# LANGUAGE OverloadedStrings #-}

module Loader
  ( loadCsv
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, toList)
import Data.Map.Ordered (fromList)
import Data.Csv (decode, HasHeader(..))
import Data.Text (Text, pack, strip)
import Data.Text.Read (decimal)
import Element (Row, Elem(..))

loadCsv :: ByteString -> Either Text [Row]
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left $ pack err
    Right recs -> Right $ extract $ deepToList recs

extract :: [[Text]] -> [Row]
extract [] = []
extract (colNames:recs) = extractRows (fmap strip colNames) recs

-- Csv Column Names -> Csv Row values -> Output Rows
extractRows :: [Text] -> [[Text]] -> [Row]
extractRows = fmap . extractRow

-- Csv Column Names -> Csv Row values -> Output Row
extractRow :: [Text] -> [Text] -> Row
extractRow colNames = fromList . zip colNames . fmap (toElem . strip)

deepToList :: Vector (Vector a) -> [[a]]
deepToList = toList . fmap toList

-- Ultimately this should be a proper value parser
toElem :: Text -> Elem
toElem "True" = BElem True
toElem "False" = BElem False
toElem s =
  case decimal s of
    Left _ -> SElem s
    Right (i, _) -> IElem i
