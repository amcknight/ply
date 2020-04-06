module Loader.Tests
  ( test1
  ) where

import Test.HUnit

test1 = TestCase (assertEqual "Test is equal" 2 2)
