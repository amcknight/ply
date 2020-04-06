
import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "Tokenizes simple query" tokenizeSimpleQuery
  ]

tokenizeSimpleQuery = assertEqual ""
  (T.tokenize "SELECT age FROM ages WHERE age = 35")
  [T.Select, T.Name "age", T.From, T.Name "ages", T.Where, T.Name "age", T.Name "=", T.Name "35"]
