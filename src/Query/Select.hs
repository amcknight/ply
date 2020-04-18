{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Query.Select
  ( Select(..)
  , Selection(..)
  , pSelect
  , evalSel
  ) where

import Name
import Parser
import Element.Elem
import Expression.Expr
import Expression.Eval
import Expression.Parse
import Table.Row (Row)
import Table.Utils (omap)
import Data.Map.Ordered (OMap)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string')
import Data.Map.Ordered as O (fromList)

data Selection = All | RowEx (OMap Name Ex) deriving (Show, Eq)
newtype Select = Select Selection deriving (Show, Eq)

evalSel :: Select -> Row -> Row
evalSel (Select All) csvRow = csvRow
evalSel (Select (RowEx rs)) csvRow = omap (evalRow csvRow) rs

evalRow :: Row -> Ex -> Elem
evalRow csvRow ex = evalEx ex csvRow

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
