{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.UpdateId.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'Arbitrary' instances for 'UpdateId'.
module Lowarn.UpdateId.Arbitrary () where

import Control.Applicative
import Lowarn.ProgramName.Arbitrary ()
import Lowarn.UpdateId
import Lowarn.VersionNumber.Arbitrary ()
import Test.QuickCheck

instance Arbitrary UpdateId where
  arbitrary :: Gen UpdateId
  arbitrary = liftA3 UpdateId arbitrary arbitrary arbitrary

  shrink :: UpdateId -> [UpdateId]
  shrink UpdateId {..} =
    liftA3
      UpdateId
      (shrink updateIdProgramName)
      (shrink updateIdPreviousVersionNumber)
      (shrink updateIdNextVersionNumber)
