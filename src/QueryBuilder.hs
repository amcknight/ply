module QueryBuilder
  ( build
  ) where

import qualified Tokenizer as T
import Parser as P
import Query as Q

-- TODO: Will need the headers for matching Cols
build :: String -> Q.Query
build = P.parse . T.tokenize
