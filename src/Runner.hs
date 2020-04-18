module Runner
  ( run
  ) where

import Query.Query
import Query.Select
import Query.Where
import Table.Table (Table, rows, buildTable)
import Table.Row
import Expression.Eval (isTrue)

run :: Query -> Table -> Table
run query tab = buildTable outRows
  where outRows = (evalSels . filterWhere) inRows
        evalSels = fmap (evalSel (select query))
        filterWhere = filter (whereFilter query)
        inRows = rows tab

whereFilter :: Query -> Row -> Bool
whereFilter query row =
  case mWhere query of
    Nothing -> True
    Just (Where ex) -> isTrue ex row
