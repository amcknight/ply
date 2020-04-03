module Parser
  ( parse
  ) where

import Tokenizer as T
import Query as Q
import Control.Exception.Base (Exception, throw)
import Data.List.Split (splitOneOf)

newtype QueryException = ParseException String deriving Show
instance Exception QueryException

parse :: [T.Token] -> Q.Query
parse tokens = case splitOneOf [T.Select, T.From, T.Where] tokens of
  ([]:[s, f, w]) -> Q.SelectFromWhere (parseSelect s) (parseFrom f) (parseWhere w)
  _ -> throw $ ParseException $ "Couldn't parse: " ++ show tokens

parseSelect :: [T.Token] -> Q.Select
parseSelect [] = throw $ ParseException "No SELECT columns given"
parseSelect ts =
  if all T.isName ts
    then Q.Select $ fmap (Q.SCol . str) ts
    else throw $ ParseException $ "All SELECT tokens should be column names but " ++ show ts ++ " was given"

parseFrom :: [T.Token] -> Q.From
parseFrom [] = throw $ ParseException "No FROM table given"
parseFrom [T.Name name] = Q.From name [Q.SCol "first_name", Q.SCol "last_name", Q.ICol "age"] -- TODO: Get rid of this hardcoding
parseFrom [t] = throw $ ParseException $ "FROM statement only accepts names but a " ++ show t ++ " was given"
parseFrom ts = throw $ ParseException $ "FROM statement currently only accepts one name, but " ++ show ts ++ "was given"

parseWhere :: [T.Token] -> Q.Where
parseWhere [] = Q.Where []
parseWhere _ = undefined -- TODO: Define
