{-# LANGUAGE OverloadedStrings #-}

module Element.Elem
  ( Elem(..)
  , ElemT(..)
  , getType
  ) where

import Data.Text (Text, unpack)

data Elem = SElem Text | IElem Int | BElem Bool deriving Eq
instance Show Elem where
  show (SElem s) = unpack s
  show (IElem i) = show i
  show (BElem b) = show b

data ElemT = SElemT | IElemT | BElemT deriving Eq
instance Show ElemT where
  show SElemT = "String"
  show IElemT = "Integer"
  show BElemT = "Boolean"

getType :: Elem -> ElemT
getType (SElem _) = SElemT
getType (IElem _) = IElemT
getType (BElem _) = BElemT
