module Element
  ( Elem(..)
  , Row
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as M

data Elem = SElem Text | IElem Int deriving Show

type Row = M.Map Text Elem
