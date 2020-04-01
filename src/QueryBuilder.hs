module QueryBuilder
  ( build
  ) where

-- TODO: This needs to work with:
-- Multiple From Strings
-- GROUPBY, HAVING, LIMIT
-- Multiple primitives rather than just String and Int (preferably as a param)
data Col = ICol String | SCol String
data EqClause = EqCC Col Col | EqCI Col Int | EdCS Col String | EqIC Int Col | EqSC String Col
newtype Select = Select [Col]
newtype From = From String
newtype Where = Where [EqClause]
data SelectFromWhere = SelectFromWhere Select From Where

-- TODO: Use the input param to build the query
build :: String -> SelectFromWhere
build _ = SelectFromWhere (Select [SCol "first_name", ICol "age"]) (From "people") (Where [EqCI (ICol "age") 35])
