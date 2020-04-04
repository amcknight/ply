module Element
  ( Elem(..)
  , Row
  ) where

import qualified Data.Map.Strict as M

data Elem = SElem String | IElem Int deriving Show

type Row = M.Map String Elem
