{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Query as Q
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string', alphaNumChar, space, space1)

type Parser = Parsec Void Text

parse :: Text -> Either Text Q.Query
parse query =
  case runParser pQuery "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q

pQuery :: Parser Q.Query
pQuery = Q.Query <$> pSelect <*> pFrom <*> pWhere <* eof
--pQuery = do
--  s <- pSelect
--  f <- pFrom
--  w <- pWhere
--  return $ Q.Query s f w

pSelect :: Parser Q.Select
pSelect = do
  _ <- string' "SELECT"
  _ <- space1
  c <- columns
  _ <- space1
  return $ Q.Select c

column :: Parser Q.Col
column = pack <$> some (alphaNumChar <|> char '_')

table :: Parser Q.Table
table = pack <$> some (alphaNumChar <|> char '_' <|> char '-' <|> char '/')

columns :: Parser [Q.Col]
columns = do
  headCol <- column
  tailCols <- (commaSpace >> columns) <|> return []
  return $ headCol : tailCols

commaSpace :: Parser ()
commaSpace = do
  _ <- char ','
  _ <- space
  return ()

pFrom :: Parser Q.From
pFrom = do
  _ <- string' "FROM"
  _ <- space1
  t <- table
  _ <- space1
  return $ Q.From t

pWhere :: Parser Q.Where
pWhere = do
  _ <- string' "WHERE"
  return Q.Where
