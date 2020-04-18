{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Name
import Query.Query
import Query.Select
import Query.From
import Expression.Expr
import Expression.Parse
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string')
import Data.Map.Ordered as O (fromList)

parse :: Text -> Either Text Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Query
pQuery = Query <$> pSelect <*> pFrom <*> optional pWhere <* eof

pWhere :: Parser Where
pWhere = Where <$> (lex1 (string' "WHERE") *> parseEx)
