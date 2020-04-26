module Query.Where
  ( Where(..)
  , pWhere
  ) where

import Expression.Expr
import Expression.Parse
import Parser
import Data.Text (Text)

newtype Where = Where Ex deriving (Show, Eq)

pWhere :: Text -> Parser Where
pWhere tName = Where <$> parseEx tName
