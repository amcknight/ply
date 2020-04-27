module Expression.Expr
  ( Ex(..)
  ) where

import Data.Text (Text)
import Name (Name)

data Ex = Var Name
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
