{-# LANGUAGE OverloadedStrings #-}

module Runner
  ( run
  ) where

import Query (Query, Col, selection, condition, evalSel)
import Element (Elem(..))
import Table (Row, Table, rows, buildTable)
import QueryException (QueryException(..))
import Data.Map.Ordered as DMO (lookup, fromList)
import Control.Exception.Base (throw)
import Expression (isTrue)

newtype MissingColumn = MissingColumn Col

-- Query -> CSV Rows -> Output Row
run :: Query -> Table -> Table
run query tab = buildTable ((fmap (evalSel query) . filter (whereFilter query)) (rows tab))

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: Query -> Row -> Row
select query csvRow =
  evalSel query csvRow
--  case selectElems selections csvRow of
--    Left (MissingColumn c) -> throw $ MissingColumnsException $ "Table was missing column " <> c
--    Right elems -> DMO.fromList $ zip (evalSel query csvRow) elems
--  where selections = selection query

selectElems :: [Col] -> Row -> Either MissingColumn [Elem]
selectElems selections csvRow = mapM (toElem csvRow) selections

-- CSV Row -> Column name -> Element
toElem :: Row -> Col -> Either MissingColumn Elem
toElem csvRow c =
  case DMO.lookup c csvRow of
    Nothing -> Left $ MissingColumn c
    Just e -> Right e

whereFilter :: Query -> Row -> Bool
whereFilter query row =
  case condition query of
    Nothing -> True
    Just cond -> isTrue cond row
