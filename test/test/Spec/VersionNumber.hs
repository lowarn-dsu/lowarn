{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionNumber (versionNumberTests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Lowarn.VersionNumber
import Lowarn.VersionNumber.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

lexicographicOrdering :: TestTree
lexicographicOrdering = testProperty (show 'lexicographicOrdering) prop
  where
    prop :: VersionNumber -> Bool
    prop v = v < fromJust (mkVersionNumber $ unVersionNumber v <> (0 :| []))

withDotsRoundTrip :: TestTree
withDotsRoundTrip =
  testProperty (show 'withDotsRoundTrip) $
    roundTripProperty showWithDots parseWithDots

withLettersRoundTrip :: TestTree
withLettersRoundTrip =
  testProperty (show 'withLettersRoundTrip) $
    roundTripProperty showWithLetters parseWithLetters

versionNumberTests :: TestTree
versionNumberTests =
  testGroup
    "Version number"
    [ lexicographicOrdering,
      withDotsRoundTrip,
      withLettersRoundTrip
    ]
