{-# LANGUAGE OverloadedStrings #-}

module Runner
  ( run
  ) where

import Query (Query, Col, selection, condition)
import Element (Row, Elem(..))
import QueryException (QueryException(..))
import Data.Map.Ordered as DMO (lookup, fromList)
import Control.Exception.Base (throw)
import Expression (isTrue)

newtype MissingColumn = MissingColumn Col

-- Query -> CSV Rows -> Output Row TODO: This is insane nonsense
run :: Query -> [Row] -> [Row]
run query = fmap (select query) . filter (whereFilter query)

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: Query -> Row -> Row
select query csvRow =
  case selectElems selections csvRow of
    Left (MissingColumn c) -> throw $ MissingColumnsException $ "Table was missing column " <> c
    Right elems -> DMO.fromList $ zip selections elems
  where selections = selection query

selectElems :: [Col] -> Row -> Either MissingColumn [Elem]
selectElems selections csvRow = mapM (toElem csvRow) selections -- TODO: Combine the MissingColumns

-- CSV Row -> Column name -> Element
toElem :: Row -> Col -> Either MissingColumn Elem
toElem csvRow c =
  case DMO.lookup c csvRow of
    Nothing -> Left $ MissingColumn c
    Just e -> Right e

whereFilter :: Query -> Row -> Bool
whereFilter query = isTrue (condition query)