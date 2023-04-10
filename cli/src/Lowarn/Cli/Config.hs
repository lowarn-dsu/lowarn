{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Config
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for Lowarn CLI configurations.
module Lowarn.Cli.Config
  ( LowarnConfig (..),
    LowarnRetrofitConfig (..),
    findConfigPath,
  )
where

import Data.Yaml
import Lowarn.Cli.Retrofit.BranchName
import Lowarn.ProgramName
import Lowarn.ProgramName.Aeson ()
import Path
import Path.IO
import URI.ByteString
import URI.ByteString.Aeson ()

-- | Type for Lowarn CLI configurations.
data LowarnConfig = LowarnConfig
  { -- | The name of the program that is being updated.
    lowarnConfigProgramName :: ProgramName,
    -- | Whether or not to attempt to unload code.
    lowarnConfigUnload :: Bool,
    -- | Whether or not to use the system linker, rather than GHC's.
    lowarnConfigSystemLinker :: Bool,
    -- | A subdirectory of each version directory to search for a @.cabal@ file
    -- in.
    lowarnConfigCabalDirectory :: Path Rel Dir,
    -- | Optional configuration for using Lowarn CLI retrofit.
    lowarnConfigRetrofitConfig :: Maybe LowarnRetrofitConfig
  }
  deriving (Eq, Show)

-- | Type for Lowarn CLI retrofit configurations.
data LowarnRetrofitConfig = LowarnRetrofitConfig
  { -- | The Git URI of the repository to clone.
    lowarnRetrofitConfigGitUri :: URIRef Absolute,
    -- | The branch to checkout.
    lowarnRetrofitConfigBranch :: BranchName
  }
  deriving (Eq, Show)

instance ToJSON LowarnConfig where
  toJSON :: LowarnConfig -> Value
  toJSON LowarnConfig {..} =
    object $
      [ "program-name" .= lowarnConfigProgramName,
        "unload" .= lowarnConfigUnload,
        "system-linker" .= lowarnConfigSystemLinker,
        "version-package-subdirectory" .= lowarnConfigCabalDirectory
      ]
        <> maybe [] (return . ("retrofit" .=)) lowarnConfigRetrofitConfig

instance FromJSON LowarnConfig where
  parseJSON :: Value -> Parser LowarnConfig
  parseJSON = withObject "LowarnConfig" $ \v ->
    LowarnConfig
      <$> v
        .: "program-name"
      <*> v
        .:? "unload"
        .!= False
      <*> v
        .:? "system-linker"
        .!= True
      <*> v
        .:? "version-package-subdirectory"
        .!= [reldir|.|]
      <*> v
        .:? "retrofit"

instance ToJSON LowarnRetrofitConfig where
  toJSON :: LowarnRetrofitConfig -> Value
  toJSON LowarnRetrofitConfig {..} =
    object
      [ "git" .= lowarnRetrofitConfigGitUri,
        "branch" .= lowarnRetrofitConfigBranch
      ]

instance FromJSON LowarnRetrofitConfig where
  parseJSON :: Value -> Parser LowarnRetrofitConfig
  parseJSON = withObject "LowarnRetrofitConfig" $ \v ->
    LowarnRetrofitConfig <$> v .: "git" <*> v .: "branch"

-- | Attempt to find a @lowarn.yaml@ file from a given search directory in a
-- given root directory. The given search directory and its ancestors are
-- searched in order, with 'Nothing' being given when there are no ancestor
-- directories with a file named @lowarn.yaml@ until the root directory is
-- reached.
findConfigPath ::
  -- | The root directory to search in.
  Path Abs Dir ->
  -- | The search directory to start searching from.
  Path Rel Dir ->
  IO (Maybe (Path Abs File))
findConfigPath rootDirectory searchDirectory =
  doesDirExist absoluteSearchDirectory >>= \case
    True ->
      doesFileExist configFilePath >>= \case
        True -> return $ Just configFilePath
        False -> do
          let searchDirectoryParent = parent searchDirectory
          if searchDirectoryParent /= searchDirectory
            then findConfigPath rootDirectory searchDirectoryParent
            else return Nothing
    False -> return Nothing
  where
    absoluteSearchDirectory = rootDirectory </> searchDirectory
    configFilePath = absoluteSearchDirectory </> [relfile|lowarn.yaml|]
