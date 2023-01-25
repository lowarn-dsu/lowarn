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

-- | Run a golden test that writes to a file when given a file path. A file path
-- is given which corresponds to the test name. The file has extension @.log@
-- and exists next to a golden file which it is compared to.
--
-- The location of the golden and log files is
-- @PACKAGE_DIR\/test\/golden\/TEST_NAME.{log,golden}@, where @PACKAGE_DIR@ is
-- the directory of the package that is being run and @TEST_NAME@ is the given
-- name of the test.
goldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- files.
  String ->
  -- | A function that takes the file path of the log file that should be
  -- written to and writes to it.
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
