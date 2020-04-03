module Runner
  ( run
  ) where

import qualified Data.Map.Strict as M
import Query as Q
import Element as E

run :: Q.Query -> [E.Row] -> [E.Row]
run (Q.SelectFromWhere (Q.Select ss) _ _) = fmap (select ss)

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: [Q.Col] -> E.Row -> E.Row
select cols = M.filterWithKey (inCols cols)

inCols :: [Q.Col] -> String -> E.Elem -> Bool
inCols selectionCols csvColName _ = csvColName `elem` fmap Q.colName selectionCols
