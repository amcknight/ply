module Loader
  ( loadCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.List
import Element as E

loadCsv :: BL.ByteString -> Either String [E.Row]
loadCsv csvData = case decode NoHeader csvData of
  Left err -> Left err
  Right v -> Right $ V.toList v
