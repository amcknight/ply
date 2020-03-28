{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( from
    , select
    ) where

import Data.ByteString.Lazy
import Data.Csv (decode, HasHeader(..))

from :: String -> From
from path = do
  csvData <- Data.ByteString.Lazy.readFile path
  case decode HasHeader csvData of
    Left err -> ["BAD"]
    Right csv -> ["Good", "Stuff"]


select :: From -> Columns -> Select
select from columns = (columns, [["YAY"]])

type Columns = [String]
type From = Columns
type Rows = (Columns, [[String]])
type Select = Rows
