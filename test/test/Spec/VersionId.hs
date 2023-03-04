{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionId (versionIdTests) where

import Lowarn.VersionId
import Lowarn.VersionId.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

versionIdRoundTrip :: TestTree
versionIdRoundTrip =
  testProperty (show 'versionIdRoundTrip) $
    roundTripProperty showVersionId parseVersionId

versionPackageNameRoundTrip :: TestTree
versionPackageNameRoundTrip =
  testProperty (show 'versionPackageNameRoundTrip) $
    roundTripProperty showVersionPackageName parseVersionPackageName

versionIdTests :: TestTree
versionIdTests =
  testGroup
    "Version ID"
    [ versionIdRoundTrip,
      versionPackageNameRoundTrip
    ]
