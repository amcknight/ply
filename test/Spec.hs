{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Query as Q
import CsvSql (runQuery)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "Run a simple query" runSimpleQuery
  , testCase "Run a simple query with reversed selection order" runSimpleQueryReversed
  , testCase "Run a query with uppercase columns" runUppercaseQuery
  ]

runSimpleQuery :: Assertion
runSimpleQuery = assertEqual ""
  "Andrew | 35\nTom | 36\n"
  (runQuery (Q.Query (Q.Select ["first_name", "age"]) (Q.From "ages") Q.Where) (BLU.fromString "first_name, last_name, age\nAndrew, McKnight, 35\nTom, Ash, 36\n"))

runSimpleQueryReversed :: Assertion
runSimpleQueryReversed = assertEqual ""
  "35 | Andrew\n36 | Tom\n"
  (runQuery (Q.Query (Q.Select ["age", "first_name"]) (Q.From "ages") Q.Where) (BLU.fromString "first_name, last_name, age\nAndrew, McKnight, 35\nTom, Ash, 36\n"))

runUppercaseQuery :: Assertion
runUppercaseQuery = assertEqual ""
  "Andrew | 35\nTom | 36\n"
  (runQuery (Q.Query (Q.Select ["FirstName", "Age"]) (Q.From "ages") Q.Where) (BLU.fromString "FirstName, LastName, Age\nAndrew, McKnight, 35\nTom, Ash, 36\n"))
