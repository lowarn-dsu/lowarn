-- |
-- Module                  : Test.Lowarn.VersionGraph
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for testing version graphs with Lowarn.
module Test.Lowarn.VersionGraph (versionGraphGoldenTest) where

import Lowarn.Cli.VersionGraph
import Lowarn.ProgramName
import Lowarn.VersionNumber
import Path hiding (Dir)
import qualified Path
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Tasty

-- | Run a golden test that writes information about a version graph to a given
-- file.
--
-- The program name and directory trees are used to write to a temporary
-- directory that is used to find a version graph. The directory trees are
-- placed in a directory with a file name of the given program name. The found
-- version graph and some information relating to it is written to the log file.
-- This function also takes the Cabal directory path argument of
-- 'getVersionGraph'.
versionGraphGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- the log and golden files.
  String ->
  -- | A program name used to make and search a program project directory.
  ProgramName ->
  -- | A relative directory path that is used for finding the @.cabal@ files
  -- of versions in version directories.
  Path Rel Path.Dir ->
  -- | Directories and empty files placed in the program project directory.
  [DirTree ()] ->
  TestTree
versionGraphGoldenTest testName programName cabalDirectory directoryChildren =
  directoryTreeGoldenTest
    testName
    (Dir programNameString directoryChildren)
    $ \logFile directoryPath -> do
      validDirectoryPath <- parseAbsDir directoryPath
      programNameDirectoryPath <- parseRelDir programNameString
      versionGraph <-
        getVersionGraph
          (validDirectoryPath </> programNameDirectoryPath)
          programName
          cabalDirectory

      let mEarliestVersionNumber = earliestVersionNumber versionGraph
      writeFile logFile $
        unlines
          [ "Version graph:",
            show versionGraph,
            "Earliest version number:",
            show $ showWithDots <$> mEarliestVersionNumber,
            "Latest version number:",
            show $ showWithDots <$> latestVersionNumber versionGraph,
            "Earliest version number after earliest version number:",
            show $
              showWithDots
                <$> ( flip earliestNextVersionNumber versionGraph
                        =<< mEarliestVersionNumber
                    ),
            "Latest version number after earliest version number:",
            show $
              showWithDots
                <$> ( flip latestNextVersionNumber versionGraph
                        =<< mEarliestVersionNumber
                    )
          ]
  where
    programNameString = unProgramName programName
