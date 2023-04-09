-- |
-- Module                  : Test.Lowarn.DirectoryTree
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for testing directory trees with Lowarn.
module Test.Lowarn.DirectoryTree (directoryTreeGoldenTest, unitFile) where

import Control.Monad
import System.Directory.Tree
import System.IO.Temp
import Test.Lowarn.Golden
import Test.Tasty

-- | Run a golden test that writes to a file when given its file path as well as
-- the file path of a temporary directory containing a given @'DirTree' ()@ that
-- has been written.
--
-- The first file path that is given corresponds to the test name. This file has
-- extension @.log@ and exists next to a golden file which it is compared to.
--
-- The second file path that is given corresponds to a temporary directory that
-- only exists during the test. This directory exists in the system's canonical
-- directory. This file path is absolute. The directory contains a directory
-- tree that is is created according to a given @'DirTree' ()@, where files are
-- created empty.
directoryTreeGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- the log and golden files.
  String ->
  -- | The directory tree that should be created inside the temporary directory.
  DirTree () ->
  -- | A function that takes the file path of the log file that should be
  -- written to and the file path of the directory containing the directory
  -- tree.
  (FilePath -> FilePath -> IO ()) ->
  TestTree
directoryTreeGoldenTest testName directoryTree f =
  goldenTest
    testName
    $ \logFile ->
      withSystemTempDirectory testName $ \temporaryDirectoryPath -> do
        void $
          writeDirectoryWith
            (const . flip writeFile "")
            (temporaryDirectoryPath :/ directoryTree)
        f logFile temporaryDirectoryPath

-- | Give a file in a @'DirTree' ()@ corresponding to a given file name.
unitFile :: FileName -> DirTree ()
unitFile = flip File ()
