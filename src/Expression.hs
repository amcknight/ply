{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Ex(..)
  , parseEx
  , evalEx
  , isTrue
  ) where

import Element
import ParseUtils
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Map.Ordered as O (lookup)

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
pVar = Var . pack <$> lex0 (some (alphaNumChar <|> char '_'))

toLitB :: Text -> Ex
toLitB "False" = LitB False
toLitB "True" = LitB True
pLitB :: Parser Ex
pLitB = toLitB <$> lex0 (string "True" <|> string "False")

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
  , [ InfixL (Add <$ lex0 (string "+"))
    , InfixL (Mul <$ lex0 (string "*"))
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

isTrue :: Ex -> Row -> Bool
isTrue ex row = evalEx ex row == Just (LitB True)

evalEx :: Ex -> Row -> Maybe Ex
evalEx (Var v) r =
  case O.lookup v r of
    Just (BElem lit) -> Just $ LitB lit
    Just (IElem lit) -> Just $ LitI lit
    Just (SElem lit) -> Just $ LitS lit
    _ -> Nothing
evalEx (LitB v) _ = Just $ LitB v
evalEx (LitI v) _ = Just $ LitI v
evalEx (LitS v) _ = Just $ LitS v
evalEx (Not e) r =
  case evalEx e r of
    Just (LitB v) -> Just $ LitB $ not v
    _ -> Nothing
evalEx (Eq e1 e2) r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitB a), Just (LitB b)) -> Just $ LitB $ a == b
    (Just (LitI a), Just (LitI b)) -> Just $ LitB $ a == b
    (Just (LitS a), Just (LitS b)) -> Just $ LitB $ a == b
    _ -> Nothing
evalEx (NEq e1 e2) r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitB a), Just (LitB b)) -> Just $ LitB $ a /= b
    (Just (LitI a), Just (LitI b)) -> Just $ LitB $ a /= b
    (Just (LitS a), Just (LitS b)) -> Just $ LitB $ a /= b
    _ -> Nothing
evalEx (And e1 e2) r = evalBBOp e1 e2 (&&) r
evalEx (Or  e1 e2) r = evalBBOp e1 e2 (||) r
evalEx (Add e1 e2) r = evalIIOp e1 e2 (+)  r
evalEx (Mul e1 e2) r = evalIIOp e1 e2 (*)  r
evalEx (Lt  e1 e2) r = evalIBOp e1 e2 (<)  r
evalEx (Gt  e1 e2) r = evalIBOp e1 e2 (>)  r
evalEx (LtE e1 e2) r = evalIBOp e1 e2 (<=) r
evalEx (GtE e1 e2) r = evalIBOp e1 e2 (>=) r
evalEx (Cat e1 e2) r = evalSSOp e1 e2 (<>) r

evalBBOp :: Ex -> Ex -> (Bool -> Bool -> Bool) -> Row -> Maybe Ex
evalBBOp e1 e2 op r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitB a), Just (LitB b)) -> Just $ LitB $ op a b
    _ -> Nothing

evalIIOp :: Ex -> Ex -> (Int -> Int -> Int) -> Row -> Maybe Ex
evalIIOp e1 e2 op r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitI a), Just (LitI b)) -> Just $ LitI $ op a b
    _ -> Nothing

evalSSOp :: Ex -> Ex -> (Text -> Text -> Text) -> Row -> Maybe Ex
evalSSOp e1 e2 op r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitS a), Just (LitS b)) -> Just $ LitS $ op a b
    _ -> Nothing

evalIBOp :: Ex -> Ex -> (Int -> Int -> Bool) -> Row -> Maybe Ex
evalIBOp e1 e2 op r =
  case (evalEx e1 r, evalEx e2 r) of
    (Just (LitI a), Just (LitI b)) -> Just $ LitB $ op a b
    _ -> Nothing
