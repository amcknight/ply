module Runner
  ( run
  ) where

import qualified Data.Map as M
import Query as Q
import Element as E

-- Query -> CSV Rows -> Output Row
run :: Q.Query -> [E.Row] -> [E.Row]
run = fmap . select . Q.selection

-- Query Select Columns -> CSV Row -> Selected CSV Row
select :: [Q.Col] -> E.Row -> E.Row
select selections = M.fromList . filter (\(k, _) -> k `elem` selections) . M.toList
