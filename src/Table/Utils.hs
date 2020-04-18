module Table.Utils
  ( omap
  , okeys
  , ovals
  ) where

import Data.Map.Ordered (OMap, assocs, fromList)

omap :: Ord a => (b -> c) -> OMap a b -> OMap a c
omap f = fromList . fmap (\(k, v) -> (k, f v)) . assocs

okeys :: OMap a b -> [a]
okeys = fmap fst . assocs

ovals :: OMap a b -> [b]
ovals = fmap snd . assocs
