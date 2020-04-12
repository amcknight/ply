module Query
  ( Query(..)
  , Select(..)
  , Col
  , From(..)
  , Table
  , Where(..)
  , selection
  , table
  ) where

import Expression (BExp)
import Data.Text (Text)

-- Select
type Col = Text
newtype Select = Select [Col] deriving (Show, Eq)

-- From
type Table = Text
newtype From = From Table deriving (Show, Eq)

-- Where
newtype Where = Where BExp deriving (Show, Eq)

data Query = Query
  { select :: Select
  , from :: From
  , whereCl :: Where
  } deriving (Show, Eq)

selection :: Query -> [Col]
selection (Query (Select ss) _ _) = ss

table :: Query -> Text
table (Query _ (From t) _) = t
