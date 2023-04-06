{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.VersionNumber.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'Arbitrary' instances for 'VersionNumber'.
module Lowarn.VersionNumber.Arbitrary () where

import Data.List.NonEmpty (fromList, toList)
import Data.Maybe
import Lowarn.VersionNumber
import Test.QuickCheck

instance Arbitrary VersionNumber where
  arbitrary :: Gen VersionNumber
  arbitrary = sized $ \n -> do
    numVersionNumberComponents <- chooseInt (1, log2 n + 1)
    fromJust . mkVersionNumber . fromList
      <$> vectorOf numVersionNumberComponents arbitrarySizedNatural
    where
      log2 :: Int -> Int
      log2 n = if n <= 1 then 0 else 1 + log2 (n `div` 2)

  shrink :: VersionNumber -> [VersionNumber]
  shrink versionNumber =
    [ fromJust $ mkVersionNumber shrunkVersionNumberComponents
      | shrunkVersionNumberComponents <-
          fromList . getNonEmpty
            <$> shrink (NonEmpty $ toList $ unVersionNumber versionNumber),
        all (>= 0) shrunkVersionNumberComponents
    ]
