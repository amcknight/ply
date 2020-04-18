module Runner
  ( run
  ) where

import Query (Query, condition, evalSel)
import Table (Row, Table, rows, buildTable)
import Expression (isTrue)

run :: Query -> Table -> Table
run query tab = buildTable outRows
  where outRows = (evalSels . filterWhere) inRows
        evalSels = fmap (evalSel query)
        filterWhere = filter (whereFilter query)
        inRows = rows tab

whereFilter :: Query -> Row -> Bool
whereFilter query row =
  case condition query of
    Nothing -> True
    Just cond -> isTrue cond row
