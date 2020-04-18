module Expression.Eval
  ( evalEx
  , isTrue
  ) where

import Expression.Expr
import Table (Row)
import Element.Elem (Elem(..))
import Data.Text
import Data.Map.Ordered as O (lookup)
import Control.Exception.Base (throw, Exception)

newtype EvalError = EvalError Ex
instance Exception EvalError
instance Show EvalError where
  show (EvalError e) = "Evaluation Error. Type checking should prevent this.Expression:\n" ++ show e

isTrue :: Ex -> Row -> Bool
isTrue ex row = eval ex row == LitB True

evalEx :: Ex -> Row -> Elem
evalEx ex row = toElem evEx row
  where evEx = eval ex row

toElem :: Ex -> Row -> Elem
toElem ex row =
  case eval ex row of
    LitB v -> BElem v
    LitI v -> IElem v
    LitS v -> SElem v
    _ -> throw $ EvalError ex

eval :: Ex -> Row -> Ex
eval ex@(Var v) r =
  case O.lookup v r of
    Just (BElem lit) -> LitB lit
    Just (IElem lit) -> LitI lit
    Just (SElem lit) -> LitS lit
    _ -> throw $ EvalError ex
eval (LitB v) _ = LitB v
eval (LitI v) _ = LitI v
eval (LitS v) _ = LitS v
eval ex@(Not e) r =
  case eval e r of
    LitB v -> LitB $ not v
    _ -> throw $ EvalError ex
eval ex@(Eq e1 e2) r =
  case (eval e1 r, eval e2 r) of
    (LitB a, LitB b) -> LitB $ a == b
    (LitI a, LitI b) -> LitB $ a == b
    (LitS a, LitS b) -> LitB $ a == b
    _ -> throw $ EvalError ex
eval ex@(NEq e1 e2) r =
  case (eval e1 r, eval e2 r) of
    (LitB a, LitB b) -> LitB $ a /= b
    (LitI a, LitI b) -> LitB $ a /= b
    (LitS a, LitS b) -> LitB $ a /= b
    _ -> throw $ EvalError ex
eval ex@(And e1 e2) r = evalBBOp ex e1 e2 (&&) r
eval ex@(Or  e1 e2) r = evalBBOp ex e1 e2 (||) r
eval ex@(Add e1 e2) r = evalIIOp ex e1 e2 (+)  r
eval ex@(Mul e1 e2) r = evalIIOp ex e1 e2 (*)  r
eval ex@(Lt  e1 e2) r = evalIBOp ex e1 e2 (<)  r
eval ex@(Gt  e1 e2) r = evalIBOp ex e1 e2 (>)  r
eval ex@(LtE e1 e2) r = evalIBOp ex e1 e2 (<=) r
eval ex@(GtE e1 e2) r = evalIBOp ex e1 e2 (>=) r
eval ex@(Cat e1 e2) r = evalSSOp ex e1 e2 (<>) r

evalBBOp :: Ex -> Ex -> Ex -> (Bool -> Bool -> Bool) -> Row -> Ex
evalBBOp ex e1 e2 op r =
  case (eval e1 r, eval e2 r) of
    (LitB a, LitB b) -> LitB $ op a b
    _ -> throw $ EvalError ex

evalIIOp :: Ex -> Ex -> Ex -> (Int -> Int -> Int) -> Row -> Ex
evalIIOp ex e1 e2 op r =
  case (eval e1 r, eval e2 r) of
    (LitI a, LitI b) -> LitI $ op a b
    _ -> throw $ EvalError ex

evalSSOp :: Ex -> Ex -> Ex -> (Text -> Text -> Text) -> Row -> Ex
evalSSOp ex e1 e2 op r =
  case (eval e1 r, eval e2 r) of
    (LitS a, LitS b) -> LitS $ op a b
    _ -> throw $ EvalError ex

evalIBOp :: Ex -> Ex -> Ex -> (Int -> Int -> Bool) -> Row -> Ex
evalIBOp ex e1 e2 op r =
  case (eval e1 r, eval e2 r) of
    (LitI a, LitI b) -> LitB $ op a b
    _ -> throw $ EvalError ex
