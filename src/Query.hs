module Query
  ( Query(..)
  , Select(..)
  , From(..)
  , Where(..)
  , Col
  , selection
  , table
  ) where
  
import Data.Text (Text)

type Col = Text
type Table = Text
newtype Select = Select [Col] deriving (Show, Eq)
newtype From = From Table deriving (Show, Eq)
data Where = Where deriving (Show, Eq)
data Query = SelectFromWhere Select From Where deriving (Show, Eq)

selection :: Query -> [Col]
selection (SelectFromWhere (Select ss) _ _) = ss

table :: Query -> Text
table (SelectFromWhere _ (From t) _) = t
