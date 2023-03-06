{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module                  : Lowarn.Cli.Config
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for Lowarn CLI configurations.
module Lowarn.Cli.Config (LowarnConfig (..)) where

import Data.Yaml
import Lowarn.ProgramName
import Lowarn.ProgramName.Aeson ()

-- | Type for Lowarn CLI configurations.
newtype LowarnConfig = LowarnConfig
  { -- | The name of the program that is being updated.
    lowarnConfigProgramName :: ProgramName
  }
  deriving (Show)

instance FromJSON LowarnConfig where
  parseJSON :: Value -> Parser LowarnConfig
  parseJSON = withObject "LowarnConfig" $ \v ->
    LowarnConfig
      <$> v
        .: "program-name"
