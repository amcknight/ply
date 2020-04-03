module Tokenizer
  ( tokenize
  , str
  , isName
  , Token(..)
  ) where

import Data.Char (toLower)

data Token = Select | From | Where | Name String deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize str = fmap (lexize . downcase) (words str)

lexize :: String -> Token
lexize str = case str of
  "select" -> Select
  "from" -> From
  "where" -> Where
  s -> Name s

downcase :: String -> String
downcase = fmap toLower

str :: Token -> String
str Select = "SELECT"
str From = "FROM"
str Where = "WHERE"
str (Name s) = s

isName :: Token -> Bool
isName (Name _) = True
isName _ = False
