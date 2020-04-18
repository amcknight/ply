{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Ex(..)
  , ExError
  , parseEx
  , evalEx
  , checkEx
  , isTrue
  ) where

import Element (Elem(..), TCol(..))
import Table
import ParseUtils
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Exception.Base (Exception)
import Control.Monad.Combinators.Expr
import Data.Map.Ordered as O (lookup)

data ExError = MissingColumnError Text
             -- Offending Expression, Expected Type, Actual Type
             | TypeError      Ex TCol TCol
             | TypeErrorLeft  Ex TCol TCol
             | TypeErrorRight Ex TCol TCol

instance Show ExError where
  show (MissingColumnError name) = "Missing column: " ++ unpack name
  show (TypeError      e ec ac) = buildTypeErrorMsg e ec ac "Type Error in "
  show (TypeErrorLeft  e ec ac) = buildTypeErrorMsg e ec ac "Type Error in left argument of "
  show (TypeErrorRight e ec ac) = buildTypeErrorMsg e ec ac "Type Error in right argument of "

buildTypeErrorMsg :: Ex -> TCol -> TCol -> String -> String
buildTypeErrorMsg e ec ac msg =
  msg ++ errMsg ++ "\n" ++ carets ++ "\nExpected " ++ show ec ++ " but found " ++ show ac
  where errMsg = show e
        carets = replicate (length msg) ' ' ++ replicate (length errMsg) '^'

instance Exception ExError

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

checkEx :: Ex -> TRow -> Either ExError TCol
checkEx (Var v) rt =
  case O.lookup v rt of
    Just t -> Right t
    Nothing -> Left $ MissingColumnError v
checkEx (LitB _) _ = Right BCol
checkEx (LitI _) _ = Right ICol
checkEx (LitS _) _ = Right SCol
checkEx (Not e) rt =
  case checkEx e rt of
    Left err -> Left err
    Right BCol -> Right BCol
    Right t -> Left $ TypeError (Not e) BCol t
checkEx (Eq e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) ->
      if t1 == t2
      then Right BCol
      else Left $ TypeErrorRight (Eq e1 e2) t1 t2 -- 2nd is always wrong on mismatch
checkEx (NEq e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) ->
      if t1 /= t2
      then Right BCol
      else Left $ TypeErrorRight (NEq e1 e2) t1 t2 -- 2nd is always wrong on mismatch
checkEx (And e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right BCol, Right BCol) -> Right BCol
    (Right BCol, Right t) -> Left $ TypeErrorRight (And e1 e2) BCol t
    (Right t, Right BCol) -> Left $ TypeErrorLeft (And e1 e2) BCol t
    (Right t, Right _) -> Left $ TypeErrorLeft (And e1 e2) BCol t -- 1st is wrong when both wrong
checkEx (Or e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right BCol, Right BCol) -> Right BCol
    (Right BCol, Right t) -> Left $ TypeErrorRight (Or e1 e2) BCol t
    (Right t, Right BCol) -> Left $ TypeErrorLeft (Or e1 e2) BCol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Or e1 e2) BCol t -- 1st is wrong when both wrong
checkEx (Add e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right ICol
    (Right ICol, Right t) -> Left $ TypeErrorRight (Add e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (Add e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Add e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (Mul e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right ICol
    (Right ICol, Right t) -> Left $ TypeErrorRight (Mul e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (Mul e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Mul e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (Lt e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right BCol
    (Right ICol, Right t) -> Left $ TypeErrorRight (Lt e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (Lt e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Lt e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (Gt e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right BCol
    (Right ICol, Right t) -> Left $ TypeErrorRight (Gt e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (Gt e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Gt e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (LtE e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right BCol
    (Right ICol, Right t) -> Left $ TypeErrorRight (LtE e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (LtE e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (LtE e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (GtE e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right ICol, Right ICol) -> Right BCol
    (Right ICol, Right t) -> Left $ TypeErrorRight (GtE e1 e2) ICol t
    (Right t, Right ICol) -> Left $ TypeErrorLeft (GtE e1 e2) ICol t
    (Right t, Right _) -> Left $ TypeErrorLeft (GtE e1 e2) ICol t -- 1st is wrong when both wrong
checkEx (Cat e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right SCol, Right SCol) -> Right SCol
    (Right SCol, Right t) -> Left $ TypeErrorRight (Cat e1 e2) SCol t
    (Right t, Right SCol) -> Left $ TypeErrorLeft (Cat e1 e2) SCol t
    (Right t, Right _) -> Left $ TypeErrorLeft (Cat e1 e2) SCol t -- 1st is wrong when both wrong
