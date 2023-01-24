-- |
-- Module                  : Test.Lowarn.Golden
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for golden testing utilities for Lowarn.
module Test.Lowarn.Golden (goldenTest) where

import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

goldenTest ::
  String ->
  (FilePath -> IO ()) ->
  TestTree
goldenTest testName f =
  goldenVsFileDiff
    testName
    (\a b -> ["diff", "-u", a, b])
    (testPath <.> "golden")
    logPath
    $ f logPath
  where
    testPath = "test" </> "golden" </> testName
    logPath = testPath <.> "log"
