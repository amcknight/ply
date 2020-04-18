module Query.Query
  ( Query(..)
  , From(..)
  , Table
  , Where(..)
  , table
  , condition
  ) where

import Expression.Expr
import Data.Text (Text)
import Query.Select

-- From
type Table = Text
newtype From = From Table deriving (Show, Eq)

-- Where
newtype Where = Where Ex deriving (Show, Eq)

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

table :: Query -> Text
table (Query _ (From t) _) = t

condition :: Query -> Maybe Ex
condition (Query _ _ (Just (Where ex))) = Just ex
condition (Query _ _ Nothing) = Nothing
