{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.VersionId.Aeson
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'ToJson' and 'FromJson' instances for 'VersionId'.
module Lowarn.VersionId.Aeson () where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Lowarn.ParserCombinators
import Lowarn.VersionId
import Text.Printf

instance ToJSON VersionId where
  toJSON :: VersionId -> Value
  toJSON = toJSON . showVersionId

  toEncoding :: VersionId -> Encoding
  toEncoding = toEncoding . showVersionId

instance FromJSON VersionId where
  parseJSON :: Value -> Parser VersionId
  parseJSON = withText "VersionId" $ \versionIdText ->
    case readWithParser parseVersionId $ unpack versionIdText of
      Just versionId -> return versionId
      Nothing -> fail $ printf "Could not parse version ID %s." versionIdText
