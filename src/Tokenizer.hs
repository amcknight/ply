module Tokenizer
  ( tokenize
  , untokenize
  , isName
  , Token(..)
  ) where

import Data.Char (toLower)

data Token = Select | From | Where | Name String deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = fmap (lexize . downcase) . words

lexize :: String -> Token
lexize "select" = Select
lexize "from" = From
lexize "where" = Where
lexize s = Name s

downcase :: String -> String
downcase = fmap toLower

untokenize :: Token -> String
untokenize Select = "SELECT"
untokenize From = "FROM"
untokenize Where = "WHERE"
untokenize (Name s) = s

isName :: Token -> Bool
isName (Name _) = True
isName _ = False
