module Query.Query
  ( Query(..)
  , pQuery
  ) where

import Query.Select
import Query.From
import Query.Where

import Text.Megaparsec hiding (State)
import ParseUtils (Parser)

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

pQuery :: Parser Query
pQuery = Query <$> pSelect <*> pFrom <*> optional pWhere