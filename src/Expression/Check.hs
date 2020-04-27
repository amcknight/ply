module Expression.Check
  ( checkEx
  , ExError
  ) where

import Table.Row (RowT)
import Expression.Expr
import Data.Map.Ordered as O (lookup)
import Element.Elem
import Control.Exception.Base (Exception)
import Data.Text (unpack)
import Name

data ExError = MissingColumnError Name
             -- Offending Expression, Expected Type, Actual Type
             | TypeError      Ex ElemT ElemT
             | TypeErrorLeft  Ex ElemT ElemT
             | TypeErrorRight Ex ElemT ElemT

instance Exception ExError
instance Show ExError where
  show (MissingColumnError n) = "Missing column: " ++ unpack (name n)
  show (TypeError      e ec ac) = buildTypeErrorMsg e ec ac "Type Error in "
  show (TypeErrorLeft  e ec ac) = buildTypeErrorMsg e ec ac "Type Error in left argument of "
  show (TypeErrorRight e ec ac) = buildTypeErrorMsg e ec ac "Type Error in right argument of "

buildTypeErrorMsg :: Ex -> ElemT -> ElemT -> String -> String
buildTypeErrorMsg e ec ac msg =
  msg ++ errMsg ++ "\n" ++ carets ++ "\nExpected " ++ show ec ++ " but found " ++ show ac
  where errMsg = show e
        carets = replicate (length msg) ' ' ++ replicate (length errMsg) '^'

checkEx :: Ex -> RowT -> Either ExError ElemT
checkEx (Var v) rt =
  case O.lookup v rt of
    Just t -> Right t
    Nothing -> Left $ MissingColumnError v
checkEx (LitB _) _ = Right BElemT
checkEx (LitI _) _ = Right IElemT
checkEx (LitS _) _ = Right SElemT
checkEx (Not e) rt =
  case checkEx e rt of
    Left err -> Left err
    Right BElemT -> Right BElemT
    Right t -> Left $ TypeError (Not e) BElemT t
checkEx (Eq e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) ->
      if t1 == t2
      then Right BElemT
      else Left $ TypeErrorRight (Eq e1 e2) t1 t2 -- 2nd is always wrong on mismatch
checkEx (NEq e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) ->
      if t1 /= t2
      then Right BElemT
      else Left $ TypeErrorRight (NEq e1 e2) t1 t2 -- 2nd is always wrong on mismatch
checkEx (And e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right BElemT, Right BElemT) -> Right BElemT
    (Right BElemT, Right t) -> Left $ TypeErrorRight (And e1 e2) BElemT t
    (Right t, Right BElemT) -> Left $ TypeErrorLeft (And e1 e2) BElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (And e1 e2) BElemT t -- 1st is wrong when both wrong
checkEx (Or e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right BElemT, Right BElemT) -> Right BElemT
    (Right BElemT, Right t) -> Left $ TypeErrorRight (Or e1 e2) BElemT t
    (Right t, Right BElemT) -> Left $ TypeErrorLeft (Or e1 e2) BElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Or e1 e2) BElemT t -- 1st is wrong when both wrong
checkEx (Add e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right IElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (Add e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (Add e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Add e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (Mul e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right IElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (Mul e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (Mul e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Mul e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (Lt e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right BElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (Lt e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (Lt e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Lt e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (Gt e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right BElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (Gt e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (Gt e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Gt e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (LtE e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right BElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (LtE e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (LtE e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (LtE e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (GtE e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IElemT, Right IElemT) -> Right BElemT
    (Right IElemT, Right t) -> Left $ TypeErrorRight (GtE e1 e2) IElemT t
    (Right t, Right IElemT) -> Left $ TypeErrorLeft (GtE e1 e2) IElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (GtE e1 e2) IElemT t -- 1st is wrong when both wrong
checkEx (Cat e1 e2) rt =
  case (checkEx e1 rt, checkEx e2 rt) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right SElemT, Right SElemT) -> Right SElemT
    (Right SElemT, Right t) -> Left $ TypeErrorRight (Cat e1 e2) SElemT t
    (Right t, Right SElemT) -> Left $ TypeErrorLeft (Cat e1 e2) SElemT t
    (Right t, Right _) -> Left $ TypeErrorLeft (Cat e1 e2) SElemT t -- 1st is wrong when both wrong
