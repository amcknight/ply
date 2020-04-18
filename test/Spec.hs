{-# LANGUAGE OverloadedStrings #-}

import CsvSql (parseAndProcess)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Data.ByteString.Lazy as B (ByteString, readFile)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Text
import System.FilePath (takeBaseName)

main :: IO ()
main = defaultMain =<< do
  g <- goldenTests
  return $ testGroup "Tests" [unitTests, g]

goldenTests :: IO TestTree
goldenTests = do
  queryPaths <- findByExtension [".sql"] "test/fixtures/queries/"
  tests <- traverse buildTest queryPaths
  return $ testGroup "Golden Test" tests

buildTest :: FilePath -> IO TestTree
buildTest queryPath = do
  let fileName = takeBaseName queryPath
  let goldenPath = "test/fixtures/golden/" ++ fileName ++ ".csv"
  queryRes <- runquery . pack . toString <$> B.readFile queryPath
  return $ goldenVsString fileName goldenPath queryRes

runquery :: Text -> IO B.ByteString
runquery query = do
  res <- parseAndProcess query
  return $ (fromString . unpack) res

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "True" alwaysTrue
  ]

alwaysTrue :: Assertion
alwaysTrue = assertEqual "" True True
