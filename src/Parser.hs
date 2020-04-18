{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Name
import Query.Query
import Query.Select
import Expression.Expr
import Expression.Parse
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)
import Data.Map.Ordered as O (fromList)

parse :: Text -> Either Text Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Query
pQuery = Query <$> pSelect <*> pFrom <*> optional pWhere <* eof

pSelect :: Parser Select
pSelect = Select <$> (lex1 (string' "SELECT") *> (star <|> columns))

star :: Parser Selection
star = All <$ lex1 (char '*')

columns :: Parser Selection
columns = RowEx . O.fromList <$> columns'

columns' :: Parser [(Name, Ex)]
columns' = do
  headCol <- column
  tailCols <- (lex0 (char ',') >> columns') <|> return empty
  return $ headCol : tailCols

column :: Parser (Name, Ex)
column =  try (parseEx >>= asColumn) <|> ((\n -> (n, Var n)) <$> pName)

asColumn :: Ex -> Parser (Name, Ex)
asColumn ex = fmap (, ex) asName

asName :: Parser Name
asName = lex1 (string' "AS") *> pName

pFrom :: Parser From
pFrom = From <$> (lex1 (string' "FROM") *> tableName)

tableName :: Parser Table
tableName = pack <$> lex1 (some (alphaNumChar <|> char '_' <|> char '-' <|> char '/'))

pWhere :: Parser Where
pWhere = Where <$> (lex1 (string' "WHERE") *> parseEx)
