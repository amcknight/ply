{-# LANGUAGE OverloadedStrings #-}

module Tokenizer
  ( tokenize
  , untokenize
  , isName
  , Token(..)
  ) where

--import Data.Char (toLower)
import Data.Text as T (Text, toLower, words)

data Token = Select | From | Where | Name Text deriving (Eq, Show)

tokenize :: Text -> [Token]
tokenize = fmap (lexize . toLower) . T.words

lexize :: Text -> Token
lexize "select" = Select
lexize "from" = From
lexize "where" = Where
lexize s = Name s

untokenize :: Token -> Text
untokenize Select = "SELECT"
untokenize From = "FROM"
untokenize Where = "WHERE"
untokenize (Name s) = s

isName :: Token -> Bool
isName (Name _) = True
isName _ = False
