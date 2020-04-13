{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Query as Q
import Expression (parseEx)
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)

parse :: Text -> Either Text Q.Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Q.Query
pQuery = Q.Query <$> pSelect <*> pFrom <*> pWhere <* eof

pSelect :: Parser Q.Select
pSelect = Q.Select <$> (lex1 (string' "SELECT") *> lex1 columns)

column :: Parser Q.Col
column = pack <$> some (alphaNumChar <|> char '_')

tableName :: Parser Q.Table
tableName = pack <$> some (alphaNumChar <|> char '_' <|> char '-' <|> char '/')

columns :: Parser [Q.Col]
columns = do
  headCol <- column
  tailCols <- (lex0 (char ',') >> columns) <|> return []
  return $ headCol : tailCols

pFrom :: Parser Q.From
pFrom = Q.From <$> (lex1 (string' "FROM") *> lex1 tableName)

pWhere :: Parser Q.Where
pWhere = Q.Where <$> (lex1 (string' "WHERE") *> parseEx)
