module Loader
  ( loadCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decode, HasHeader(..))
import Data.Map as M (fromList)
import Data.Maybe (fromMaybe)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Element as E

loadCsv :: BL.ByteString -> Either String [E.Row]
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left err
    Right recs -> Right $ extract $ deepToList recs

extract :: [[String]] -> [E.Row]
extract (colNames:recs) = extractRows (fmap trim colNames) recs

-- Csv Column Names -> Csv Row values -> Output Rows
extractRows :: [String] -> [[String]] -> [E.Row]
extractRows = fmap . extractOne

-- Csv Column Names -> Csv Row values -> Output Row
extractOne :: [String] -> [String] -> E.Row
extractOne colNames = M.fromList . zip colNames . fmap toElem

deepToList :: V.Vector (V.Vector a) -> [[a]]
deepToList = V.toList . fmap V.toList

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

toElem :: String -> Elem
toElem s = case readMaybe s of
  Nothing -> E.SElem s
  Just i -> E.IElem i
