{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( BExp
  ) where

import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

--data SSB = Equal deriving (Show, Eq)
--data SSS = Concat deriving (Show, Eq)
data SExp = SLit Text
          | SVar Text
          | Concat SExp SExp
          deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

binary :: Text -> (SExp -> SExp -> SExp) -> Operator Parser SExp
binary name f = InfixL (f <$ symbol name)
--

pLit :: Parser SExp
pLit = SLit . pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

pVar :: Parser SExp
pVar = SVar . pack <$> some alphaNumChar

pTerm :: Parser SExp
pTerm = choice
  [ pLit
  , pVar
  ]

pExp :: Parser SExp
pExp = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser SExp]]
operatorTable =
  [ [ binary "+" Concat
    ]
  ]

--pSSB :: Parser SSB
--pSSB = choice [Equal <$ char '=']
--
--pSSS :: Parser SSS
--pSSS = choice [Concat <$ string "++"]
--
--pSExp :: Parser SExp
--pSExp = choice
--  [ SLit <$> pQuotedText
--  , SCol <$> pCol
--  , pSExp'
--  ]
--
--pSExp' = do
--  e1 <- pSExp
--  _ <- space
--  o <- pSSS
--  _ <- space
--  e2 <- pSExp
--  return $ SSS e1 o e2
--
--
--pCol :: Parser Text
--pCol = pack <$> some (alphaNumChar <|> char '_')
--
--pQuotedText :: Parser Text
--pQuotedText = pack <$> (char '"' *> manyTill charLiteral (char '"'))

--data III = Add | Sub | Mul | Div deriving (Show, Eq)
--data IExp = ILit Int
--          | ICol Text
--          | III IExp III IExp
--          deriving (Show, Eq)
--
--type IIB = Ordering
--data BLit = False | True deriving (Show, Eq)
--data BB = Not deriving (Show, Eq)
--data BBB = And | Or deriving (Show, Eq)
data BExp = BLit
--          | BB BB BExp
--          | BBB BExp BBB BExp
--          | IIB IExp IIB IExp
--          | SSB SExp SSB SExp
          deriving (Show, Eq)