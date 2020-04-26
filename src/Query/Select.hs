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
import Text.Megaparsec.Char (char)
import Data.Map.Ordered as O (fromList)
import Data.Text (Text)

data Selection = All | RowEx (OMap Name Ex) deriving (Show, Eq)
newtype Select = Select Selection deriving (Show, Eq)

evalSel :: Select -> Row -> Row
evalSel (Select All) csvRow = csvRow
evalSel (Select (RowEx rs)) csvRow = omap (evalRow csvRow) rs

evalRow :: Row -> Ex -> Elem
evalRow csvRow ex = evalEx ex csvRow

pSelect :: Text -> Parser Select
pSelect tName = Select <$> (pStar <|> pColumns tName)

pStar :: Parser Selection
pStar = All <$ lex1 (char '*')

data Column = Column Ex Name deriving Show

toPair :: Column -> (Name, Ex)
toPair (Column e n) = (n, e)

pColumns :: Text -> Parser Selection
pColumns tName = RowEx . O.fromList . fmap toPair <$> pColumns' tName

pColumns' :: Text -> Parser [Column]
pColumns' tName = do
  headCol <- pColumn tName
  tailCols <- (lex0 (char ',') >> pColumns' tName) <|> return empty
  return $ headCol : tailCols

pColumn :: Text -> Parser Column
pColumn tName = try (parseEx tName >>= pAsColumn tName) <|> (toColumn <$> pName tName)

pAsColumn :: Text -> Ex -> Parser Column
pAsColumn tName e = fmap (Column e) (asName tName)

toColumn :: Name -> Column
toColumn n = Column (Var n) n
