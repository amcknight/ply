module Tokenizer
  ( tokenize
  , Token(..)
  ) where

import Data.Char (toLower)

data Token = Select | From | Where | Name deriving Show

tokenize :: String -> [Token]
tokenize str = fmap (lexize . downcase) (words str)

lexize :: String -> Token
lexize str = case str of
  "select" -> Select
  "from" -> From
  "where" -> Where
  s -> Name

downcase :: String -> String
downcase = fmap toLower
