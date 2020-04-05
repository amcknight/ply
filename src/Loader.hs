module Loader
  ( loadCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decode, HasHeader(..))
import Data.Map as M (fromList)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Element as E
import Control.Exception.Base (Exception, throw)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

loadCsv :: BL.ByteString -> Either String [E.Row]
loadCsv csvData =
  case decode NoHeader csvData of
    Left err -> Left err
    Right recs -> Right $ extract $ V.toList $ fmap V.toList recs

extract :: [[String]] -> [E.Row]
extract (colNames:recs) = fmap (extractOne (fmap trim colNames)) recs

trim = dropWhileEnd isSpace . dropWhile isSpace

extractOne :: [String] -> [String] -> E.Row
extractOne colNames values = M.fromList $ zip colNames $ fmap toElem values

toElem :: String -> Elem
toElem s = case readMaybe s of
  Nothing -> E.SElem s
  Just i -> E.IElem i
