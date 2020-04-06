module Query
  ( Query(..)
  , Select(..)
  , From(..)
  , Where(..)
  , Col
  , selection
  , table
  ) where

type Col = String
type Table = String
newtype Select = Select [Col] deriving (Show, Eq)
newtype From = From Table deriving (Show, Eq)
data Where = Where deriving (Show, Eq)
data Query = SelectFromWhere Select From Where deriving (Show, Eq)

selection :: Query -> [Col]
selection (SelectFromWhere (Select ss) _ _) = ss

table :: Query -> String
table (SelectFromWhere _ (From t) _) = t
