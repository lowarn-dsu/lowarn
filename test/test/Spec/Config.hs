{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Config (configTests) where

import Lowarn.Cli.Config
import Path hiding (Dir)
import qualified Path
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Tasty

findConfigGoldenTest ::
  String -> Path Rel Path.Dir -> Path Rel Path.Dir -> TestTree
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
      strippedSearchDirectory <-
        if rootDirectory `isProperPrefixOf` searchDirectory
          then stripProperPrefix rootDirectory searchDirectory
          else
            return $
              if rootDirectory == searchDirectory
                then [reldir|.|]
                else searchDirectory

      configPath <-
        findConfigPath
          (validDirectoryPath </> rootDirectory)
          strippedSearchDirectory

      strippedConfigPath <-
        maybe
          (return Nothing)
          (fmap Just . stripProperPrefix validDirectoryPath)
          configPath

      writeFile logFile $ unlines ["Config path:", show strippedConfigPath]

findConfigBase :: TestTree
findConfigBase =
  findConfigGoldenTest
    (show 'findConfigBase)
    [reldir|.|]
    [reldir|following|]

findConfigRootMissing :: TestTree
findConfigRootMissing =
  findConfigGoldenTest
    (show 'findConfigRootMissing)
    [reldir|.|]
    [reldir|.|]

findConfigRootFound :: TestTree
findConfigRootFound =
  findConfigGoldenTest
    (show 'findConfigRootFound)
    [reldir|following|]
    [reldir|following|]

findConfigNested :: TestTree
findConfigNested =
  findConfigGoldenTest
    (show 'findConfigNested)
    [reldir|.|]
    [reldir|following/versions/1.0.0|]

findConfigNonExistent :: TestTree
findConfigNonExistent =
  findConfigGoldenTest
    (show 'findConfigNonExistent)
    [reldir|.|]
    [reldir|following/versions/2.0.0|]

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ findConfigBase,
      findConfigRootMissing,
      findConfigRootFound,
      findConfigNested,
      findConfigNonExistent
    ]