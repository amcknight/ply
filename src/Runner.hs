module Runner
  ( run
  ) where

import qualified Data.Map as M
import Query as Q
import Element as E

-- Query -> CSV Rows -> Output Row
run :: Q.Query -> [E.Row] -> [E.Row]
run (Q.SelectFromWhere (Q.Select ss) _ _) = fmap $ select ss

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: [Q.Col] -> E.Row -> E.Row
select selections csvRow = M.fromList $ filter (\(k, _) -> k `elem` selections) (M.toList csvRow)


--select selections csvRow = M.filterWithKey (inCols selections) csvRow
--
--inCols :: [Q.Col] -> String -> E.Elem -> Bool
--inCols selections csvColName _ = elem csvColName selections
