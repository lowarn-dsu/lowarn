-- |
-- Module                  : Lowarn.VersionId.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'Arbitrary' instances for 'VersionId'.
module Lowarn.VersionId.Arbitrary () where

import Lowarn.ProgramName.Arbitrary ()
import Lowarn.VersionId (VersionId (VersionId))
import Lowarn.VersionNumber.Arbitrary ()
import Test.QuickCheck

instance Arbitrary VersionId where
  arbitrary = VersionId <$> arbitrary <*> arbitrary

  shrink (VersionId programName versionNumber) = do
    programName' <- shrink programName
    versionNumber' <- shrink versionNumber
    return $ VersionId programName' versionNumber'
