{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.UpdateId (updateIdTests) where

import Lowarn.UpdateId
import Lowarn.UpdateId.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

updateIdRoundTrip :: TestTree
updateIdRoundTrip =
  testProperty (show 'updateIdRoundTrip) $
    roundTripProperty showUpdateId parseUpdateId

updatePackageNameRoundTrip :: TestTree
updatePackageNameRoundTrip =
  testProperty (show 'updatePackageNameRoundTrip) $
    roundTripProperty showUpdatePackageName parseUpdatePackageName

updateIdTests :: TestTree
updateIdTests =
  testGroup
    "Update ID"
    [ updateIdRoundTrip,
      updatePackageNameRoundTrip
    ]
