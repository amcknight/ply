module Query.Query
  ( Query(..)
  , parse
  ) where

import Query.Select
import Query.From
import Query.Where
import Query.RawQuery
import Parser
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import Data.Text

data Query = Query
  { select :: Select
  , from :: From
  , mWhere :: Maybe Where
  } deriving (Show, Eq)

-- TODO: Can this be improved with <*> ?
parse :: Text -> Either Text Query
parse query =
  case runParser (pRawQuery <* eof) "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right rq -> case transQuery rq of
      Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
      Right q -> Right q

transQuery :: RawQuery -> Either ParserError Query
transQuery (RawQuery rs rf mrw) = do
  f@(From (TableName _ tn)) <- runParser pFrom "" rf
  s <- runParser (pSelect tn) "" rs
  case mrw of
    Nothing -> return $ Query s f Nothing
    Just rw -> do
      w <- runParser (pWhere tn) "" rw
      return $ Query s f (Just w)
