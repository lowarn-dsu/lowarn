{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.VersionNumber.Aeson
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'Data.Aeson.ToJson' and 'Data.Aeson.FromJson' instances for
-- 'Lowarn.VersionNumber.VersionNumber'.
module Lowarn.VersionNumber.Aeson () where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Lowarn.ParserCombinators
import Lowarn.VersionNumber
import Text.Printf

instance ToJSON VersionNumber where
  toJSON :: VersionNumber -> Value
  toJSON = toJSON . showWithDots

  toEncoding :: VersionNumber -> Encoding
  toEncoding = toEncoding . showWithDots

instance FromJSON VersionNumber where
  parseJSON :: Value -> Parser VersionNumber
  parseJSON = withText "VersionNumber" $ \versionNumberText ->
    case readWithParser parseWithDots $ unpack versionNumberText of
      Just versionNumber -> return versionNumber
      Nothing ->
        fail $
          printf "Could not parse version number %s." versionNumberText
