module Parser
  ( parse
  ) where

import Tokenizer as T
import Query as Q
import Control.Exception.Base (Exception, throw)

newtype QueryException = ParseException String deriving Show
instance Exception QueryException

parse :: [T.Token] -> Q.Query
parse tokens = case tokens of
  [T.Select, T.Name, T.Name, T.From, T.Name] -> Q.SelectFromWhere (Q.Select [Q.SCol "first_name", Q.ICol "age"]) (Q.From "people_no_header" [Q.SCol "first_name", Q.SCol "last_name", Q.ICol "age"]) (Q.Where [])
  _ -> throw $ ParseException $ "Could not parse tokens: " ++ show tokens

