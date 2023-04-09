{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Lowarn.VersionGraph (versionGraphGoldenTest) where

import Lowarn.Cli.VersionGraph
import Lowarn.ProgramName
import Lowarn.VersionNumber
import Path hiding (Dir)
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Tasty

-- | Run a golden test that writes information about a version graph to a given
-- file.
--
-- The first file path that is given corresponds to the test name. This file has
-- extension @.log@ and exists next to a golden file which it is compared to.
--
-- The program name and directory trees are used to write to a temporary
-- directory that is used to find a version graph. The directory trees are
-- placed in a directory with a file name of the given program name. The found
-- version graph and some information relating to it is written to the log file.
versionGraphGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- files.
  String ->
  -- | A program name used to make and search a program project directory.
  ProgramName ->
  -- | Directories and empty files placed in the program project directory.
  [DirTree ()] ->
  TestTree
versionGraphGoldenTest testName programName directoryChildren =
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
