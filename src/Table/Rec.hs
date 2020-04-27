module Table.Rec
  ( Rec
  , toRecs
  ) where

import Data.Text (Text)
import Data.Vector (Vector, toList)

type Rec = [Text]

toRecs :: Vector (Vector Text) -> [Rec]
toRecs = toList . fmap toRec

toRec :: Vector Text -> Rec
toRec = toList
