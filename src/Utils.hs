module Utils
  ( omap
  ) where

import Data.Map.Ordered (OMap, assocs, fromList)

omap :: Ord a => (b -> c) -> OMap a b -> OMap a c
omap f om = fromList $ fmap (\(k, v) -> (k, f v)) (assocs om)
