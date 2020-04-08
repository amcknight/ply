{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse
  ) where

import Tokenizer as T (Token(..), isName, untokenize)
import Query as Q (Query(..), Select(..), From(..), Where(..))
import Control.Exception.Base (throw)
import Data.List.Split (splitOneOf)
import Data.Text as DT (pack)
import QueryException (QueryException(..))

parse :: [Token] -> Q.Query
parse tokens = case splitOneOf [T.Select, T.From, T.Where] tokens of
  ([]:[s, f, w]) -> SelectFromWhere (parseSelect s) (parseFrom f) (parseWhere w)
  _ -> throw $ ParseException $ "Couldn't parse: " <> pack (show tokens)

parseSelect :: [T.Token] -> Q.Select
parseSelect [] = throw $ ParseException "No SELECT columns given"
parseSelect ts =
  if all T.isName ts
    then Q.Select $ fmap untokenize ts
    else throw $ ParseException $ "All SELECT tokens should be column names but " <> pack (show ts) <> " was given"

parseFrom :: [T.Token] -> Q.From
parseFrom [] = throw $ ParseException "No FROM table given"
parseFrom [T.Name name] = Q.From name
parseFrom [t] = throw $ ParseException $ "FROM statement only accepts names but a " <> pack (show t) <> " was given"
parseFrom ts = throw $ ParseException $ "FROM statement currently only accepts one name, but " <> pack (show ts) <> "was given"

parseWhere :: [T.Token] -> Q.Where
parseWhere [] = Q.Where
parseWhere _ = undefined
