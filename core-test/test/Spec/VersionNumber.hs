{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionNumber (versionNumberTests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Lowarn.VersionNumber (VersionNumber, mkVersionNumber, unVersionNumber)
import Lowarn.VersionNumber.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

versionNumberOrder :: TestTree
versionNumberOrder = testProperty (show 'versionNumberOrder) prop
  where
    prop :: VersionNumber -> Bool
    prop v = v < fromJust (mkVersionNumber $ unVersionNumber v <> (0 :| []))

versionNumberTests :: TestTree
versionNumberTests =
  testGroup
    "Version number tests"
    [ versionNumberOrder
    ]
