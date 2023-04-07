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

jsonVersionIdRoundTrip :: TestTree
jsonVersionIdRoundTrip =
  testProperty (show 'jsonVersionIdRoundTrip) $
    jsonRoundTripProperty (Proxy :: Proxy VersionId)

yamlVersionIdRoundTrip :: TestTree
yamlVersionIdRoundTrip =
  testProperty (show 'yamlVersionIdRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy VersionId)

versionIdTests :: TestTree
versionIdTests =
  testGroup
    "Version ID"
    [ versionIdRoundTrip,
      versionPackageNameRoundTrip,
      jsonVersionIdRoundTrip,
      yamlVersionIdRoundTrip
    ]
