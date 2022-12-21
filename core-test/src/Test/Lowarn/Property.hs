module Test.Lowarn.Property (roundTripProperty) where

import Lowarn.ParserCombinators (readWithParser)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    propertyForAllShrinkShow,
  )
import Text.ParserCombinators.ReadP (ReadP)

roundTripProperty :: (Arbitrary a, Eq a) => (a -> String) -> ReadP a -> Property
roundTripProperty customShow customParse =
  propertyForAllShrinkShow arbitrary shrink (return . customShow) $
    \a -> Just a == readWithParser customParse (customShow a)
