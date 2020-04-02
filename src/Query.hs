module Query
  ( Query(..)
  , Select(..)
  , From(..)
  , Where(..)
  , EqClause(..)
  , Col(..)
  , table
  ) where

-- TODO: This needs:
-- Multiple From Strings or Join
-- include JOIN, GROUP BY, HAVING, LIMIT, ORDER BY
-- Multiple primitives rather than just String and Int (preferably as a param)
data Col = ICol String | SCol String
data EqClause = EqCC Col Col | EqCI Col Int | EdCS Col String | EqIC Int Col | EqSC String Col
newtype Select = Select [Col]
data From = From String [Col]
newtype Where = Where [EqClause]
data Query = SelectFromWhere Select From Where

table :: Query -> String
table (SelectFromWhere _ (From t _) _) = t
