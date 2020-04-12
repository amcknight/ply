module Query
  ( Query(..)
  , Select(..)
  , From(..)
  , Where(..)
  , Col
  , Table
  , selection
  , table
  ) where
  
import Data.Text (Text)

type Col = Text
newtype Select = Select [Col] deriving (Show, Eq)

type Table = Text
newtype From = From Table deriving (Show, Eq)

data Where = Where deriving (Show, Eq)

data Query = Query
  { select :: Select
  , from :: From
  , whereCl :: Where
  } deriving (Show, Eq)

selection :: Query -> [Col]
selection (Query (Select ss) _ _) = ss

table :: Query -> Text
table (Query _ (From t) _) = t
