{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Config (configTests) where

import Control.Applicative
import Data.Aeson
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Yaml
import Lowarn.Cli.Config
import Lowarn.ProgramName.Arbitrary ()
import Path hiding (Dir)
import qualified Path
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Lowarn.Golden
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.RawString.QQ

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

readConfigGoldenTest :: String -> String -> TestTree
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

readDefaultConfig :: TestTree
readDefaultConfig =
  readConfigGoldenTest
    (show 'readDefaultConfig)
    [r|
program-name: following
|]

readNonDefaultConfig :: TestTree
readNonDefaultConfig =
  readConfigGoldenTest
    (show 'readNonDefaultConfig)
    [r|
program-name: following
unload: true
system-linker: false
|]

readEmptyConfig :: TestTree
readEmptyConfig = readConfigGoldenTest (show 'readEmptyConfig) "\n"

newtype ArbitraryLowarnConfig = ArbitraryLowarnConfig LowarnConfig
  deriving (Eq)
  deriving newtype (ToJSON, FromJSON)

instance Arbitrary ArbitraryLowarnConfig where
  arbitrary :: Gen ArbitraryLowarnConfig
  arbitrary =
    ArbitraryLowarnConfig <$> liftA3 LowarnConfig arbitrary arbitrary arbitrary

  shrink :: ArbitraryLowarnConfig -> [ArbitraryLowarnConfig]
  shrink (ArbitraryLowarnConfig (LowarnConfig {..})) =
    ArbitraryLowarnConfig
      <$> liftA3
        LowarnConfig
        (shrink lowarnConfigProgramName)
        [lowarnConfigUnload]
        [lowarnConfigSystemLinker]

yamlConfigRoundTrip :: TestTree
yamlConfigRoundTrip =
  testProperty (show 'yamlConfigRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy ArbitraryLowarnConfig)

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ findConfigBase,
      findConfigRootMissing,
      findConfigRootFound,
      findConfigNested,
      findConfigNonExistent,
      readDefaultConfig,
      readNonDefaultConfig,
      readEmptyConfig,
      yamlConfigRoundTrip
    ]
