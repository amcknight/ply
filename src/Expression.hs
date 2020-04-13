{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Ex
  , pEx
  , parseEx
  ) where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

data Ex = Var Text
        | LitB Bool
        | LitI Int
        | LitS Text
        | Not Ex
        | Eq  Ex Ex
        | NEq Ex Ex
        | And Ex Ex
        | Or  Ex Ex
        | Add Ex Ex
        | Mul Ex Ex
        | Lt  Ex Ex
        | Gt  Ex Ex
        | LtE Ex Ex
        | GtE Ex Ex
        | Cat Ex Ex
        deriving (Show, Eq)

pVar :: Parser Ex
pVar = Var . pack <$> lexeme (some alphaNumChar)

toLitB :: Text -> Ex
toLitB "False" = LitB False
toLitB "True" = LitB True
pLitB :: Parser Ex
pLitB = toLitB <$> lexeme (string "True" <|> string "False")

pLitI :: Parser Ex
pLitI = LitI <$> lexeme L.decimal

pLitS :: Parser Ex
pLitS = LitS . pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pTerm :: Parser Ex
pTerm = choice
  [ pLitB
  , pLitI
  , pLitS
  , pVar
  ]

ops :: [[Operator Parser Ex]]
ops =
  [ [ Prefix (Not <$ lexeme (string "!"))
    ]
  , [ InfixL (And <$ lexeme (string "&"))
    , InfixL (Or  <$ lexeme (string "|"))
    ]
  , [ InfixL (Cat <$ lexeme (string "++"))
    ]
  , [ InfixL (Add <$ lexeme (string "+"))
    , InfixL (Mul <$ lexeme (string "*"))
    ]
  , [ InfixL (LtE <$ lexeme (string "<="))
    , InfixL (GtE <$ lexeme (string ">="))
    ]
  , [ InfixL (Lt  <$ lexeme (string "<"))
    , InfixL (Gt  <$ lexeme (string ">"))
    ]
  , [ InfixL (Eq  <$ lexeme (string "="))
    , InfixL (NEq <$ lexeme (string "!="))
    ]
  ]

pEx :: Parser Ex
pEx = makeExprParser pTerm ops

parseEx :: Text -> Either Text Ex
parseEx query =
  case runParser pEx "" query of
    Left errorBundle -> Left $ pack $ errorBundlePretty errorBundle
    Right q -> Right q
