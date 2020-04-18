module Query.Query
  ( Query(..)
  , pQuery
  , parse
  ) where

import Query.Select
import Query.From
import Query.Where
import Parser
import Text.Megaparsec (runParser, eof, optional, errorBundlePretty)
import Data.Text

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

parse :: Text -> Either Text Query
parse query =
  case runParser (pQuery <* eof) "SQL Query Parser" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Query
pQuery = Query <$> pSelect <*> pFrom <*> optional pWhere
