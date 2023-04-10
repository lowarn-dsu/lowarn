{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Config (configTests) where

import Control.Monad
import Data.Maybe
import Data.Proxy
import Lowarn.Cli.Config
import Lowarn.Cli.Retrofit.BranchName
import Lowarn.ProgramName.Arbitrary ()
import Path hiding (Dir)
import Test.Lowarn.Config
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.RawString.QQ
import URI.ByteString

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

retrofit:
  git: https://github.com/xmonad/xmonad.git
  branch: master
|]

readEmptyConfig :: TestTree
readEmptyConfig = readConfigGoldenTest (show 'readEmptyConfig) "\n"

instance Arbitrary LowarnConfig where
  arbitrary :: Gen LowarnConfig
  arbitrary = liftM4 LowarnConfig arbitrary arbitrary arbitrary arbitrary

  shrink :: LowarnConfig -> [LowarnConfig]
  shrink LowarnConfig {..} =
    liftM4
      LowarnConfig
      (shrink lowarnConfigProgramName)
      [lowarnConfigUnload]
      [lowarnConfigSystemLinker]
      (shrink lowarnConfigRetrofitConfig)

instance Arbitrary LowarnRetrofitConfig where
  arbitrary :: Gen LowarnRetrofitConfig
  arbitrary = do
    uriScheme <- Scheme <$> elements ["http", "https"]

    let authorityUserInfo = Nothing
    authorityHost <- Host <$> elements ["github.com", "gitlab.com"]
    authorityPort <- elements [Nothing, Just $ Port 80, Just $ Port 443]
    let uriAuthority = Just Authority {..}

    uriPath <- elements ["/xmonad/xmonad.git", "/"]

    let uriQuery = Query []
        uriFragment = Nothing

    let lowarnRetrofitConfigGitUri = URI {..}

    lowarnRetrofitConfigBranch <-
      fromJust . mkBranchName <$> listOf1 (elements ['a' .. 'z'])

    return LowarnRetrofitConfig {..}

yamlConfigRoundTrip :: TestTree
yamlConfigRoundTrip =
  testProperty (show 'yamlConfigRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy LowarnConfig)

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
