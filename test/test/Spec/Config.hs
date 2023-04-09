{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Config (configTests) where

import Control.Applicative
import Data.Aeson
import Data.Proxy
import Lowarn.Cli.Config
import Lowarn.ProgramName.Arbitrary ()
import Path hiding (Dir)
import Test.Lowarn.Config
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.RawString.QQ

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
    [reldir|.|]

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
  deriving (Eq, ToJSON, FromJSON)

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
