{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module                  : Test.Lowarn.VersionGraph
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for testing CLI configurations with Lowarn.
module Test.Lowarn.Config (findConfigGoldenTest, readConfigGoldenTest) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Yaml
import Lowarn.Cli.Config
import Path hiding (Dir)
import qualified Path
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Lowarn.Golden
import Test.Tasty

-- | Run a golden test that writes information about finding a CLI configuration
-- file from a given directory to a given file.

-- The first file path is the root directory where we place a directory tree
-- that looks like a program project directory. The second file path is a
-- directory relative to this root directory where we search for a CLI
-- configuration file.
findConfigGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- the log and golden files.
  String ->
  -- | The root directory given to 'findConfigPath', relative to the temporary
  -- directory created by the test.
  Path Rel Path.Dir ->
  -- | The search directory given to 'findConfigPath'.
  Path Rel Path.Dir ->
  TestTree
findConfigGoldenTest testName rootDirectory searchDirectory =
  directoryTreeGoldenTest
    testName
    ( Dir
        "following"
        [ Dir "versions" [Dir "1.0.0" []],
          Dir "updates" [],
          unitFile "lowarn.yaml"
        ]
    )
    $ \logFile directoryPath -> do
      validDirectoryPath <- parseAbsDir directoryPath

      configPath <-
        findConfigPath (validDirectoryPath </> rootDirectory) searchDirectory

      strippedConfigPath <-
        maybe
          (return Nothing)
          (fmap Just . stripProperPrefix validDirectoryPath)
          configPath

      writeFile logFile $ unlines ["Config path:", show strippedConfigPath]

-- | Run a golden test that writes a given CLI configuration in YAML format to a
-- given file.
readConfigGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- the log and golden files.
  String ->
  -- | The contents of the CLI configuration file.
  String ->
  TestTree
readConfigGoldenTest testName configFileContents =
  goldenTest
    testName
    $ \logFile -> do
      let eLowarnConfig =
            decodeEither' @LowarnConfig $
              Text.encodeUtf8 $
                Text.pack
                  configFileContents
      writeFile logFile $ unlines ["Config:", show eLowarnConfig]
