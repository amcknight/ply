
import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer as T
import Parser as P
import Query as Q

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "Tokenizes simple query" tokenizeSimpleQuery
  , testCase "Parse simple tokens" parseSimpleTokens
  ]

tokenizeSimpleQuery = assertEqual ""
  (T.tokenize "SELECT age FROM ages WHERE age = 35")
  [T.Select, T.Name "age", T.From, T.Name "ages", T.Where, T.Name "age", T.Name "=", T.Name "35"]

parseSimpleTokens = assertEqual ""
  (P.parse [T.Select, T.Name "age", T.From, T.Name "ages", T.Where])
  (Q.SelectFromWhere (Q.Select ["age"]) (Q.From "ages") Q.Where)
