
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [ testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1,2,3] ]
