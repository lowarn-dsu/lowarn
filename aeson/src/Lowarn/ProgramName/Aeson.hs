{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.ProgramName.Aeson
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'ToJson' and 'FromJson' instances for 'ProgramName'.
module Lowarn.ProgramName.Aeson () where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Lowarn.ProgramName
import Text.Printf

instance ToJSON ProgramName where
  toJSON :: ProgramName -> Value
  toJSON = toJSON . unProgramName

  toEncoding :: ProgramName -> Encoding
  toEncoding = toEncoding . unProgramName

instance FromJSON ProgramName where
  parseJSON :: Value -> Parser ProgramName
  parseJSON = withText "ProgramName" $ \programNameText ->
    case mkProgramName $ unpack programNameText of
      Just programName -> return programName
      Nothing ->
        fail $ printf "Could not parse program name %s." programNameText
