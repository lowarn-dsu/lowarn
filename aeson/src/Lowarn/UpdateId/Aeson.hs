{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.UpdateId.Aeson
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'Data.Aeson.ToJson' and 'Data.Aeson.FromJson' instances for
-- 'Lowarn.UpdateId.UpdateId'.
module Lowarn.UpdateId.Aeson () where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Lowarn.ParserCombinators
import Lowarn.UpdateId
import Text.Printf

instance ToJSON UpdateId where
  toJSON :: UpdateId -> Value
  toJSON = toJSON . showUpdateId

  toEncoding :: UpdateId -> Encoding
  toEncoding = toEncoding . showUpdateId

instance FromJSON UpdateId where
  parseJSON :: Value -> Parser UpdateId
  parseJSON = withText "UpdateId" $ \updateIdText ->
    case readWithParser parseUpdateId $ unpack updateIdText of
      Just updateId -> return updateId
      Nothing -> fail $ printf "Could not parse update ID %s." updateIdText
