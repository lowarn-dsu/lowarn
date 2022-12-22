{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionId (versionIdTests) where

import Lowarn.VersionId
  ( parseVersionId,
    parseVersionPackageName,
    showVersionId,
    showVersionPackageName,
  )
import Lowarn.VersionId.Arbitrary ()
import Test.Lowarn.Property (roundTripProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

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
