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
tokenize = fmap lexize . T.words

lexize :: Text -> Token
lexize s
  | toLower s == "select" = Select
  | toLower s == "from" = From
  | toLower s == "where" = Where
  | otherwise = Name s

untokenize :: Token -> Text
untokenize Select = "SELECT"
untokenize From = "FROM"
untokenize Where = "WHERE"
untokenize (Name s) = s

isName :: Token -> Bool
isName (Name _) = True
isName _ = False
