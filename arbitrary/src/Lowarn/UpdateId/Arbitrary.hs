-- |
-- Module                  : Lowarn.UpdateId.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'Arbitrary' instances for 'UpdateId'.
module Lowarn.UpdateId.Arbitrary () where

import Lowarn.ProgramName.Arbitrary ()
import Lowarn.UpdateId (UpdateId (UpdateId))
import Lowarn.VersionNumber.Arbitrary ()
import Test.QuickCheck

instance Arbitrary UpdateId where
  arbitrary = UpdateId <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (UpdateId programName previousVersionNumber nextVersionNumber) =
    do
      programName' <- shrink programName
      previousVersionNumber' <- shrink previousVersionNumber
      nextVersionNumber' <- shrink nextVersionNumber
      return $
        UpdateId programName' previousVersionNumber' nextVersionNumber'
