{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Query as Q
import Expression (parseEx, Ex(..))
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar)
import Data.Map.Ordered as O (fromList)
import Text.Megaparsec.Debug (dbg)


parse :: Text -> Either Text Q.Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Q.Query
pQuery = Q.Query <$> pSelect <*> pFrom <*> optional pWhere <* eof

pSelect :: Parser Q.Select
pSelect = Q.Select <$> (lex1 (string' "SELECT") *> columns)

columns :: Parser Q.Selection
columns = O.fromList <$> columns'

columns' :: Parser [(Q.Col, Ex)]
columns' = do
  headCol <- column -- <|> asColumn
  tailCols <- (lex0 (char ',') >> columns') <|> return empty
  return $ headCol : tailCols

column :: Parser (Q.Col, Ex)
column = do
  ex <- parseEx
  case ex of
    (Var name) -> return (name, ex)
    _ -> asColumn ex

asColumn :: Ex -> Parser (Q.Col, Ex)
asColumn ex = do
  _ <- lex1 (string' "AS")
  name <- lex0 $ some $ alphaNumChar <|> char '_'
  return (pack name, ex)

pFrom :: Parser Q.From
pFrom = Q.From <$> (lex1 (string' "FROM") *> lex1 tableName)

tableName :: Parser Q.Table
tableName = pack <$> some (alphaNumChar <|> char '_' <|> char '-' <|> char '/')

pWhere :: Parser Q.Where
pWhere = Q.Where <$> (lex1 (string' "WHERE") *> parseEx)
