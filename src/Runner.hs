module Runner
  ( run
  ) where

import Query as Q

type Row = (String, String, Int)

run :: Q.Query -> [Row] -> [[String]]
run (Q.SelectFromWhere (Q.Select ss) _ _) = fmap (select ss)

select :: [Q.Col] -> Row -> [String]
select selections (firstName, lastName, age) = [firstName, show age] -- TODO: Actually use the column selections