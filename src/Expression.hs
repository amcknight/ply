{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( BExp
  , parseBoolExp
  ) where

import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

data SExp = SLit Text
          | SVar Text
          | Concat SExp SExp
          deriving (Show, Eq)
--

pSLit :: Parser SExp
pSLit = SLit . pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSVar :: Parser SExp
pSVar = SVar . pack <$> lexeme (some alphaNumChar)

pSTerm :: Parser SExp
pSTerm = choice
  [ pSLit
  , pSVar
  ]

sOperatorTable :: [[Operator Parser SExp]]
sOperatorTable =
  [ [ InfixL (Concat <$ lexeme (string "+"))
    ]
  ]

pSExp :: Parser SExp
pSExp = makeExprParser pSTerm sOperatorTable
--

data IExp = ILit Int
          | IVar Text
          | Add IExp IExp
          | Sub IExp IExp
          | Mul IExp IExp
          | Div IExp IExp
          deriving (Show, Eq)

pILit :: Parser IExp
pILit = ILit <$> lexeme L.decimal

pIVar :: Parser IExp
pIVar = IVar . pack <$> lexeme (some alphaNumChar)

pITerm :: Parser IExp
pITerm = choice
  [ pILit
  , pIVar
  ]

iOperatorTable :: [[Operator Parser IExp]]
iOperatorTable =
  [ [ InfixL (Mul <$ lexeme (string "*"))
    , InfixL (Div <$ lexeme (string "/"))
    ]
  , [ InfixL (Add <$ lexeme (string "+"))
    , InfixL (Sub <$ lexeme (string "-"))
    ]
  ]

pIExp :: Parser IExp
pIExp = makeExprParser pITerm iOperatorTable
--

data BExp = BLit Bool
          | BVar Text
          | Not BExp
          | EqB BExp BExp
          | And BExp BExp
          | Or  BExp BExp
          | EqI IExp IExp
          | LtI IExp IExp
          | GtI IExp IExp
          | EqS SExp SExp
          deriving (Show, Eq)

toBLit :: Text -> BExp
toBLit "False" = BLit False
toBLit "True" = BLit True

pBLit :: Parser BExp
pBLit = toBLit <$> lexeme (string "True" <|> string "False")

pBVar :: Parser BExp
pBVar = BVar . pack <$> lexeme (some alphaNumChar)

pBTerm :: Parser BExp
pBTerm = choice
  [ pBLit
  , pBVar
  ]

bOperatorTable :: [[Operator Parser BExp]]
bOperatorTable =
  [ [ InfixL (EqB <$ lexeme (string "="))
    , InfixL (EqI <$ lexeme (string "="))
    , InfixL (EqS <$ lexeme (string "="))
    ]
  , [ InfixL (And <$ lexeme (string "&"))
    , InfixL (Or  <$ lexeme (string "|"))
    ]
  , [ InfixL (LtI <$ lexeme (string "<"))
    , InfixL (GtI <$ lexeme (string ">"))
    ]
  ]

pBExp :: Parser BExp
pBExp = makeExprParser pBTerm bOperatorTable
--

parseBoolExp :: Parser BExp
parseBoolExp = undefined