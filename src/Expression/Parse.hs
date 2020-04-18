{-# LANGUAGE OverloadedStrings #-}

module Expression.Parse
  ( parseEx
  ) where

import Expression.Expr (Ex(..))
import ParseUtils (Parser, lex0)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Text (pack)

-- TODO: Duplicated in Parser under colName. Should bring together in a common col/var/header module
pVar :: Parser Ex
pVar = Var . pack <$> lex0 (some (alphaNumChar <|> char '_'))

pLitB :: Parser Ex
pLitB = true <|> false
true :: Parser Ex
true = LitB True <$ lex0 (string "True")
false :: Parser Ex
false = LitB False <$ lex0 (string "False")

pLitI :: Parser Ex
pLitI = LitI <$> lex0 L.decimal

pLitS :: Parser Ex
pLitS = LitS . pack <$> lex0 (char '"' >> manyTill L.charLiteral (char '"'))

pTerm :: Parser Ex
pTerm = choice
  [ pLitB
  , pLitI
  , pLitS
  , pVar
  ]

ops :: [[Operator Parser Ex]]
ops =
  [ [ Prefix (Not <$ lex0 (string' "NOT"))
    ]
  , [ InfixL (Cat <$ lex0 (string "++"))
    ]
  , [ InfixL (Mul <$ lex0 (string "*"))
    ]
  , [ InfixL (Add <$ lex0 (string "+"))
    ]
  , [ InfixL (LtE <$ lex0 (string "<="))
    , InfixL (GtE <$ lex0 (string ">="))
    ]
  , [ InfixL (Lt  <$ lex0 (string "<"))
    , InfixL (Gt  <$ lex0 (string ">"))
    ]
  , [ InfixL (Eq  <$ lex0 (string "="))
    , InfixL (NEq <$ lex0 (string "!="))
    ]
  , [ InfixL (And <$ lex0 (string' "AND"))
    , InfixL (Or  <$ lex0 (string' "OR"))
    ]
  ]

parseEx :: Parser Ex
parseEx = makeExprParser pTerm ops
