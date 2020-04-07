{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer as T
import Parser as P
import Query as Q
import CsvSql (buildQuery, runQuery)
import Data.Text (pack)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "Tokenizes simple query" tokenizeSimpleQuery
  , testCase "Parse simple tokens" parseSimpleTokens
  , testCase "Build a simple query" buildSimpleQuery
  , testCase "Run a simple query" runSimpleQuery
  ]

tokenizeSimpleQuery :: Assertion
tokenizeSimpleQuery = assertEqual ""
  [T.Select, T.Name "age", T.From, T.Name "ages", T.Where, T.Name "age", T.Name "=", T.Name "35"]
  (T.tokenize "SELECT age FROM ages WHERE age = 35")

parseSimpleTokens :: Assertion
parseSimpleTokens = assertEqual ""
  (Q.SelectFromWhere (Q.Select ["age"]) (Q.From "ages") Q.Where)
  (P.parse [T.Select, T.Name "age", T.From, T.Name "ages", T.Where])

buildSimpleQuery :: Assertion
buildSimpleQuery = assertEqual ""
  (Q.SelectFromWhere (Q.Select ["first_name", "age"]) (Q.From "ages") Q.Where)
  (buildQuery "SELECT first_name age FROM ages WHERE")

runSimpleQuery :: Assertion
runSimpleQuery = assertEqual ""
  "Andrew | 35\nTom | 36\n"
  (runQuery (Q.SelectFromWhere (Q.Select ["first_name", "age"]) (Q.From "ages") Q.Where) (BLU.fromString "first_name, last_name, age\nAndrew, McKnight, 35\nTom, Ash, 36\n"))
