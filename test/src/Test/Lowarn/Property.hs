-- |
-- Module                  : Test.Lowarn.Property
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for property testing utilities for Lowarn.
module Test.Lowarn.Property (roundTripProperty) where

import Lowarn.ParserCombinators (readWithParser)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    propertyForAllShrinkShow,
  )
import Text.ParserCombinators.ReadP (ReadP)

-- | Give the property that the result of parsing the result of showing a value,
-- with a given custom parser and show function, is the original value.
roundTripProperty :: (Arbitrary a, Eq a) => (a -> String) -> ReadP a -> Property
roundTripProperty customShow customParse =
  propertyForAllShrinkShow arbitrary shrink (return . customShow) $
    \a -> Just a == readWithParser customParse (customShow a)
