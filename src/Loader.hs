module Loader
  ( loadCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.List

loadCsv :: BL.ByteString -> Either String [(String, String, Int)]
loadCsv csvData = case decode NoHeader csvData of
  Left err -> Left err
  Right v -> Right $ V.toList v
