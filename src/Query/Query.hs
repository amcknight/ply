module Query.Query
  ( Query(..)
  , Where(..)
  , condition
  ) where

import Expression.Expr
import Data.Text (Text)
import Query.Select
import Query.From

-- Where
newtype Where = Where Ex deriving (Show, Eq)

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

condition :: Query -> Maybe Ex
condition (Query _ _ (Just (Where ex))) = Just ex
condition (Query _ _ Nothing) = Nothing
