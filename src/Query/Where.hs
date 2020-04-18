{-# LANGUAGE OverloadedStrings #-}

module Query.Where
  ( Where(..)
  , pWhere
  ) where

import Expression.Expr
import Expression.Parse
import Text.Megaparsec.Char (string')
import Parser (Parser, lex1)

newtype Where = Where Ex deriving (Show, Eq)

pWhere :: Parser Where
pWhere = Where <$> (lex1 (string' "WHERE") *> parseEx)
