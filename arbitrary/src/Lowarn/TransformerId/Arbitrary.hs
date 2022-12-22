-- |
-- Module                  : Lowarn.TransformerId.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'Arbitrary' instances for 'TransformerId'.
module Lowarn.TransformerId.Arbitrary () where

import Lowarn.ProgramName.Arbitrary ()
import Lowarn.TransformerId (TransformerId (TransformerId))
import Lowarn.VersionNumber.Arbitrary ()
import Test.QuickCheck

instance Arbitrary TransformerId where
  arbitrary = TransformerId <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (TransformerId programName previousVersionNumber nextVersionNumber) =
    do
      programName' <- shrink programName
      previousVersionNumber' <- shrink previousVersionNumber
      nextVersionNumber' <- shrink nextVersionNumber
      return $
        TransformerId programName' previousVersionNumber' nextVersionNumber'
