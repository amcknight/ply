module Utils
  ( omap
  , okeys
  , ovals
  ) where

import Data.Map.Ordered (OMap, assocs, fromList)

omap :: Ord a => (b -> c) -> OMap a b -> OMap a c
omap f om = fromList $ fmap (\(k, v) -> (k, f v)) (assocs om)

okeys :: OMap a b -> [a]
okeys = fmap fst . assocs

ovals :: OMap a b -> [b]
ovals = fmap snd . assocs