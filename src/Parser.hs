{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import qualified Query as Q
import Expression.Expr (Ex(..))
import Expression.Parse
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)
import Data.Map.Ordered as O (fromList)

parse :: Text -> Either Text Q.Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Q.Query
pQuery = Q.Query <$> pSelect <*> pFrom <*> optional pWhere <* eof

pSelect :: Parser Q.Select
pSelect = Q.Select <$> (lex1 (string' "SELECT") *> (star <|> columns))

star :: Parser Q.Selection
star = Q.All <$ lex1 (char '*')

columns :: Parser Q.Selection
columns = Q.RowEx . O.fromList <$> columns'

columns' :: Parser [(Q.Col, Ex)]
columns' = do
  headCol <- column
  tailCols <- (lex0 (char ',') >> columns') <|> return empty
  return $ headCol : tailCols

column :: Parser (Q.Col, Ex)
column =  try (parseEx >>= asColumn) <|> ((\n -> (n, Var n)) <$> colName)

asColumn :: Ex -> Parser (Q.Col, Ex)
asColumn ex = fmap (, ex) asName

asName :: Parser Q.Col
asName = lex1 (string' "AS") *> colName

colName :: Parser Q.Col
colName = pack <$> lex0 (some (alphaNumChar <|> char '_'))

pFrom :: Parser Q.From
pFrom = Q.From <$> (lex1 (string' "FROM") *> tableName)

tableName :: Parser Q.Table
tableName = pack <$> lex1 (some (alphaNumChar <|> char '_' <|> char '-' <|> char '/'))

pWhere :: Parser Q.Where
pWhere = Q.Where <$> (lex1 (string' "WHERE") *> parseEx)
