{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionId (versionIdTests) where

import Data.Proxy
import Lowarn.VersionId
import Lowarn.VersionId.Aeson ()
import Lowarn.VersionId.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

versionIdRoundTrip :: TestTree
versionIdRoundTrip =
  testProperty (show 'versionIdRoundTrip) $
    parserCombinatorRoundTripProperty showVersionId parseVersionId

versionPackageNameRoundTrip :: TestTree
versionPackageNameRoundTrip =
  testProperty (show 'versionPackageNameRoundTrip) $
    parserCombinatorRoundTripProperty
      showVersionPackageName
      parseVersionPackageName

encodedVersionIdRoundTrip :: TestTree
encodedVersionIdRoundTrip =
  testProperty (show 'encodedVersionIdRoundTrip) $
    aesonRoundTripProperty (Proxy :: Proxy VersionId)

versionIdTests :: TestTree
versionIdTests =
  testGroup
    "Version ID"
    [ versionIdRoundTrip,
      versionPackageNameRoundTrip,
      encodedVersionIdRoundTrip
    ]
