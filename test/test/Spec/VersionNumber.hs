{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionNumber (versionNumberTests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Lowarn.VersionNumber
  ( VersionNumber,
    mkVersionNumber,
    parseWithDots,
    parseWithLetters,
    showWithDots,
    showWithLetters,
    unVersionNumber,
  )
import Lowarn.VersionNumber.Arbitrary ()
import Test.Lowarn.Property (roundTripProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

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
