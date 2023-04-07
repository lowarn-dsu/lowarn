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

-- | Attempt to find a @lowarn.yaml@ file from the current working directory.
-- The current working directory and its ancestors are searched in order, with
-- 'Nothing' being given when there are no ancestor directories with the file
-- @lowarn.yaml@.
findConfigPath :: IO (Maybe (Path Abs File))
findConfigPath = getCurrentDir >>= findConfigFileWithParent
  where
    findConfigFileWithParent :: Path Abs Dir -> IO (Maybe (Path Abs File))
    findConfigFileWithParent parentDir =
      let grandparentDir = parent parentDir
       in if grandparentDir == parentDir
            then return Nothing
            else do
              let configFilePath = parentDir </> [relfile|lowarn.yaml|]
              doesFileExist configFilePath >>= \case
                True -> return $ Just configFilePath
                False -> findConfigFileWithParent grandparentDir
