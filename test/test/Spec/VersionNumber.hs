{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionNumber (versionNumberTests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Proxy
import Lowarn.VersionNumber
import Lowarn.VersionNumber.Aeson ()
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
    parserCombinatorRoundTripProperty showWithDots parseWithDots

withLettersRoundTrip :: TestTree
withLettersRoundTrip =
  testProperty (show 'withLettersRoundTrip) $
    parserCombinatorRoundTripProperty showWithLetters parseWithLetters

jsonVersionNumberRoundTrip :: TestTree
jsonVersionNumberRoundTrip =
  testProperty (show 'jsonVersionNumberRoundTrip) $
    jsonRoundTripProperty (Proxy :: Proxy VersionNumber)

yamlVersionNumberRoundTrip :: TestTree
yamlVersionNumberRoundTrip =
  testProperty (show 'yamlVersionNumberRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy VersionNumber)

versionNumberTests :: TestTree
versionNumberTests =
  testGroup
    "Version number"
    [ lexicographicOrdering,
      withDotsRoundTrip,
      withLettersRoundTrip,
      jsonVersionNumberRoundTrip,
      yamlVersionNumberRoundTrip
    ]
