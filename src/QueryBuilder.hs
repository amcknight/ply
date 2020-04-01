module QueryBuilder
  ( build
  , table
  ) where

-- TODO: This needs to work with:
-- Multiple From Strings or Join
-- include JOIN, GROUP BY, HAVING, LIMIT, ORDER BY
-- Multiple primitives rather than just String and Int (preferably as a param)
data Col = ICol String | SCol String
data EqClause = EqCC Col Col | EqCI Col Int | EdCS Col String | EqIC Int Col | EqSC String Col
newtype Select = Select [Col]
newtype From = From String
newtype Where = Where [EqClause]
data Query = SelectFromWhere Select From Where

build :: String -> Query
build str = SelectFromWhere (Select [SCol "first_name", ICol "age"]) (From "people_no_header") (Where [EqCI (ICol "age") 35])
--build str = parse $ tokenize str

data Token = String

tokenize :: String -> [Token]
tokenize str = undefined

parse :: [Token] -> Query
parse tokens = undefined

table :: Query -> String
table (SelectFromWhere _ (From t) _) = t
