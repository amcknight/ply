module Query
  ( Query(..)
  , Select(..)
  , From(..)
  , Where(..)
  , EqClause(..)
  , Col(..)
  , table
  , colName
  ) where

-- TODO: This needs:
-- Multiple From Strings or Join
-- include JOIN, GROUP BY, HAVING, LIMIT, ORDER BY
-- Multiple primitives rather than just String and Int (preferably as a param)
data Col = ICol String | SCol String deriving Show
data EqClause = EqCC Col Col | EqCI Col Int | EdCS Col String | EqIC Int Col | EqSC String Col deriving Show
newtype Select = Select [Col] deriving Show
data From = From String [Col] deriving Show
newtype Where = Where [EqClause] deriving Show
data Query = SelectFromWhere Select From Where deriving Show

table :: Query -> String
table (SelectFromWhere _ (From t _) _) = t

colName :: Col -> String
colName (ICol s) = s
colName (SCol s) = s
