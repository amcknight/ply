{-# LANGUAGE OverloadedStrings #-}

module Runner
  ( run
  ) where

import Query (Query, Col, selection)
import Element (Row, Elem(..))
import QueryException (QueryException(..))
import Data.Map.Ordered as DMO (lookup, fromList)
import Control.Exception.Base (throw)

newtype MissingColumn = MissingColumn Col

-- Query -> CSV Rows -> Output Row TODO: This is insane nonsense
run :: Query -> [Row] -> [Row]
run = fmap . select . selection

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: [Col] -> Row -> Row
select selections csvRow =
  case selectElems selections csvRow of
    Left (MissingColumn c) -> throw $ MissingColumnsException $ "Table was missing column " <> c
    Right elems -> DMO.fromList $ zip selections elems

selectElems :: [Col] -> Row -> Either MissingColumn [Elem]
selectElems selections csvRow = mapM (toElem csvRow) selections -- TODO: Combine the MissingColumns

-- CSV Row -> Column name -> Element
toElem :: Row -> Col -> Either MissingColumn Elem
toElem csvRow c =
  case DMO.lookup c csvRow of
    Nothing -> Left $ MissingColumn c
    Just e -> Right e
