{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.VersionId.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'Arbitrary' instances for 'VersionId'.
module Lowarn.VersionId.Arbitrary () where

import Control.Applicative
import Lowarn.ProgramName.Arbitrary ()
import Lowarn.VersionId
import Lowarn.VersionNumber.Arbitrary ()
import Test.QuickCheck

instance Arbitrary VersionId where
  arbitrary :: Gen VersionId
  arbitrary = VersionId <$> arbitrary <*> arbitrary

  shrink :: VersionId -> [VersionId]
  shrink VersionId {..} =
    liftA2
      VersionId
      (shrink versionIdProgramName)
      (shrink versionIdVersionNumber)
