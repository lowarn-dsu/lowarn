{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module                  : Lowarn.Cli.Config
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for Lowarn CLI configurations.
module Lowarn.Cli.Config (LowarnConfig (..), findConfigPath) where

import Data.Yaml
import Lowarn.ProgramName
import Lowarn.ProgramName.Aeson ()
import Path
import Path.IO

-- | Type for Lowarn CLI configurations.
data LowarnConfig = LowarnConfig
  { -- | The name of the program that is being updated.
    lowarnConfigProgramName :: ProgramName,
    -- | Whether or not to attempt to unload code.
    lowarnConfigUnload :: Bool,
    -- | Whether or not to use the system linker, rather than GHC's.
    lowarnConfigSystemLinker :: Bool
  }
  deriving (Show)

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
